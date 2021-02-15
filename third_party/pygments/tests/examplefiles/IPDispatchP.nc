/*
 * "Copyright (c) 2008 The Regents of the University  of California.
 * All rights reserved."
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice, the following
 * two paragraphs and the author appear in all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS."
 *
 */

#include <lib6lowpan/blip-tinyos-includes.h>
#include <lib6lowpan/6lowpan.h>
#include <lib6lowpan/lib6lowpan.h>
#include <lib6lowpan/ip.h>
#include <lib6lowpan/in_cksum.h>
#include <lib6lowpan/ip_malloc.h>

#include "blip_printf.h"
#include "IPDispatch.h"
#include "BlipStatistics.h"
#include "table.h"

/*
 * Provides IP layer reception to applications on motes.
 *
 * @author Stephen Dawson-Haggerty <stevedh@cs.berkeley.edu>
 */

module IPDispatchP {
  provides {
    interface SplitControl;
    // interface for protocols not requiring special hand-holding
    interface IPLower;

    interface BlipStatistics<ip_statistics_t>;

  }
  uses {
    interface Boot;


    /* link-layer wiring */
    interface SplitControl as RadioControl;

    interface Packet as BarePacket;
    interface Send as Ieee154Send;
    interface Receive as Ieee154Receive;

    /* context lookup */
    interface NeighborDiscovery;

    interface ReadLqi;
    interface PacketLink;
    interface LowPowerListening;

    /* buffers for outgoing fragments */
    interface Pool<message_t> as FragPool;
    interface Pool<struct send_info> as SendInfoPool;
    interface Pool<struct send_entry> as SendEntryPool;
    interface Queue<struct send_entry *> as SendQueue;

    /* expire reconstruction */
    interface Timer<TMilli> as ExpireTimer;

    interface Leds;

  }
  provides interface Init;
} implementation {

#define HAVE_LOWPAN_EXTERN_MATCH_CONTEXT
int lowpan_extern_read_context(struct in6_addr *addr, int context) {
  return call NeighborDiscovery.getContext(context, addr);
}

int lowpan_extern_match_context(struct in6_addr *addr, uint8_t *ctx_id) {
  return call NeighborDiscovery.matchContext(addr, ctx_id);
}

  // generally including source files like this is a no-no.  I'm doing
  // this in the hope that the optimizer will do a better job when
  // they're part of a component.
#include <lib6lowpan/ieee154_header.c>
#include <lib6lowpan/lib6lowpan.c>
#include <lib6lowpan/lib6lowpan_4944.c>
#include <lib6lowpan/lib6lowpan_frag.c>

  enum {
    S_RUNNING,
    S_STOPPED,
    S_STOPPING,
  };
  uint8_t state = S_STOPPED;
  bool radioBusy;
  uint8_t current_local_label = 0;
  ip_statistics_t stats;

  // this in theory could be arbitrarily large; however, it needs to
  // be large enough to hold all active reconstructions, and any tags
  // which we are dropping.  It's important to keep dropped tags
  // around for a while, or else there are pathological situations
  // where you continually allocate buffers for packets which will
  // never complete.

  ////////////////////////////////////////
  //
  //

  table_t recon_cache;

  // table of packets we are currently receiving fragments from, that
  // are destined to us
  struct lowpan_reconstruct recon_data[N_RECONSTRUCTIONS];

  //
  //
  ////////////////////////////////////////

  // task void sendTask();

  void reconstruct_clear(void *ent) {
    struct lowpan_reconstruct *recon = (struct lowpan_reconstruct *)ent;
    memclr((uint8_t *)&recon->r_meta, sizeof(struct ip6_metadata));
    recon->r_timeout = T_UNUSED;
    recon->r_buf = NULL;
  }

  struct send_info *getSendInfo() {
    struct send_info *ret = call SendInfoPool.get();
    if (ret == NULL) return ret;
    ret->_refcount = 1;
    ret->upper_data = NULL;
    ret->failed = FALSE;
    ret->link_transmissions = 0;
    ret->link_fragments = 0;
    ret->link_fragment_attempts = 0;
    return ret;
  }
#define SENDINFO_INCR(X) ((X)->_refcount)++
void SENDINFO_DECR(struct send_info *si) {
  if (--(si->_refcount) == 0) {
    call SendInfoPool.put(si);
  }
}

  command error_t SplitControl.start() {
    return call RadioControl.start();
  }

  command error_t SplitControl.stop() {
    if (!radioBusy) {
      state = S_STOPPED;
      return call RadioControl.stop();
    } else {
      // if there's a packet in the radio, wait for it to exit before
      // stopping
      state = S_STOPPING;
      return SUCCESS;
    }
  }

  event void RadioControl.startDone(error_t error) {
#ifdef LPL_SLEEP_INTERVAL
    call LowPowerListening.setLocalWakeupInterval(LPL_SLEEP_INTERVAL);
#endif

    if (error == SUCCESS) {
      call Leds.led2Toggle();
      call ExpireTimer.startPeriodic(FRAG_EXPIRE_TIME);
      state = S_RUNNING;
      radioBusy = FALSE;
    }

    signal SplitControl.startDone(error);
  }

  event void RadioControl.stopDone(error_t error) {
    signal SplitControl.stopDone(error);
  }

  command error_t Init.init() {
    // ip_malloc_init needs to be in init, not booted, because
    // context for coap is initialised in init
    ip_malloc_init();
    return SUCCESS;
  }

  event void Boot.booted() {
    call BlipStatistics.clear();

    /* set up our reconstruction cache */
    table_init(&recon_cache, recon_data, sizeof(struct lowpan_reconstruct), N_RECONSTRUCTIONS);
    table_map(&recon_cache, reconstruct_clear);

    call SplitControl.start();
  }

  /*
   *  Receive-side code.
   */ 
  void deliver(struct lowpan_reconstruct *recon) {
    struct ip6_hdr *iph = (struct ip6_hdr *)recon->r_buf;

    // printf("deliver [%i]: ", recon->r_bytes_rcvd);
    // printf_buf(recon->r_buf, recon->r_bytes_rcvd);

    /* the payload length field is always compressed, have to put it back here */
    iph->ip6_plen = htons(recon->r_bytes_rcvd - sizeof(struct ip6_hdr));
    signal IPLower.recv(iph, (void *)(iph + 1), &recon->r_meta);

    // printf("ip_free(%p)\n", recon->r_buf);
    ip_free(recon->r_buf);
    recon->r_timeout = T_UNUSED;
    recon->r_buf = NULL;
  }

  /*
   * Bulletproof recovery logic is very important to make sure we
   * don't get wedged with no free buffers.
   * 
   * The table is managed as follows:
   *  - unused entries are marked T_UNUSED
   *  - entries which 
   *     o have a buffer allocated
   *     o have had a fragment reception before we fired
   *     are marked T_ACTIVE
   *  - entries which have not had a fragment reception during the last timer period
   *     and were active are marked T_ZOMBIE
   *  - zombie receptions are deleted: their buffer is freed and table entry marked unused.
   *  - when a fragment is dropped, it is entered into the table as T_FAILED1.
   *     no buffer is allocated
   *  - when the timer fires, T_FAILED1 entries are aged to T_FAILED2.
   * - T_FAILED2 entries are deleted.  Incomming fragments with tags
   *     that are marked either FAILED1 or FAILED2 are dropped; this
   *     prevents us from allocating a buffer for a packet which we
   *     have already dropped fragments from.
   *
   */ 
  void reconstruct_age(void *elt) {
    struct lowpan_reconstruct *recon = (struct lowpan_reconstruct *)elt;
    if (recon->r_timeout != T_UNUSED) 
      printf("recon src: 0x%x tag: 0x%x buf: %p recvd: %i/%i\n", 
                 recon->r_source_key, recon->r_tag, recon->r_buf, 
                 recon->r_bytes_rcvd, recon->r_size);
    switch (recon->r_timeout) {
    case T_ACTIVE:
      recon->r_timeout = T_ZOMBIE; break; // age existing receptions
    case T_FAILED1:
      recon->r_timeout = T_FAILED2; break; // age existing receptions
    case T_ZOMBIE:
    case T_FAILED2:
      // deallocate the space for reconstruction
      printf("timing out buffer: src: %i tag: %i\n", recon->r_source_key, recon->r_tag);
      if (recon->r_buf != NULL) {
        printf("ip_free(%p)\n", recon->r_buf);
        ip_free(recon->r_buf);
      }
      recon->r_timeout = T_UNUSED;
      recon->r_buf = NULL;
      break;
    }
  }

  void ip_print_heap() {
#ifdef PRINTFUART_ENABLED
    bndrt_t *cur = (bndrt_t *)heap;
    while (((uint8_t *)cur)  - heap < IP_MALLOC_HEAP_SIZE) {
      printf ("heap region start: %p length: %u used: %u\n", 
                  cur, (*cur & IP_MALLOC_LEN), (*cur & IP_MALLOC_INUSE) >> 15);
      cur = (bndrt_t *)(((uint8_t *)cur) + ((*cur) & IP_MALLOC_LEN));
    }
#endif
  }

  event void ExpireTimer.fired() {
    table_map(&recon_cache, reconstruct_age);

    
    printf("Frag pool size: %i\n", call FragPool.size());
    printf("SendInfo pool size: %i\n", call SendInfoPool.size());
    printf("SendEntry pool size: %i\n", call SendEntryPool.size());
    printf("Forward queue length: %i\n", call SendQueue.size());
    ip_print_heap();
    printfflush();
  }

  /*
   * allocate a structure for recording information about incomming fragments.
   */

  struct lowpan_reconstruct *get_reconstruct(uint16_t key, uint16_t tag) {
    struct lowpan_reconstruct *ret = NULL;
    int i;

    // printf("get_reconstruct: %x %i\n", key, tag);

    for (i = 0; i < N_RECONSTRUCTIONS; i++) {
      struct lowpan_reconstruct *recon = (struct lowpan_reconstruct *)&recon_data[i];

      if (recon->r_tag == tag &&
          recon->r_source_key == key) {

        if (recon->r_timeout > T_UNUSED) {          
          recon->r_timeout = T_ACTIVE;
          ret = recon;
          goto done;

        } else if (recon->r_timeout < T_UNUSED) {
          // if we have already tried and failed to get a buffer, we
          // need to drop remaining fragments.
          ret = NULL;
          goto done;
        }
      }
      if (recon->r_timeout == T_UNUSED) 
        ret = recon;
    }
  done:
    // printf("got%p\n", ret);
    return ret;
  }

  event message_t *Ieee154Receive.receive(message_t *msg, void *msg_payload, uint8_t len) {
    struct packed_lowmsg lowmsg;
    struct ieee154_frame_addr frame_address;
    uint8_t *buf = msg_payload;

    // printf(" -- RECEIVE -- len : %i\n", len);

    BLIP_STATS_INCR(stats.rx_total);

    /* unpack the 802.15.4 address fields */
    buf  = unpack_ieee154_hdr(msg_payload, &frame_address);
    len -= buf - (uint8_t *)msg_payload;

    /* unpack and 6lowpan headers */
    lowmsg.data = buf;
    lowmsg.len  = len;
    lowmsg.headers = getHeaderBitmap(&lowmsg);
    if (lowmsg.headers == LOWMSG_NALP) {
      goto fail;
    }

    if (hasFrag1Header(&lowmsg) || hasFragNHeader(&lowmsg)) {
      // start reassembly
      int rv;
      struct lowpan_reconstruct *recon;
      uint16_t tag, source_key;

      source_key = ieee154_hashaddr(&frame_address.ieee_src);
      getFragDgramTag(&lowmsg, &tag);
      recon = get_reconstruct(source_key, tag);
      if (!recon) {
        goto fail;
      }

      /* fill in metadata: on fragmented packets, it applies to the
         first fragment only  */
      memcpy(&recon->r_meta.sender, &frame_address.ieee_src,
             sizeof(ieee154_addr_t));
      recon->r_meta.lqi = call ReadLqi.readLqi(msg);
      recon->r_meta.rssi = call ReadLqi.readRssi(msg);

      if (hasFrag1Header(&lowmsg)) {
        if (recon->r_buf != NULL) goto fail;
        rv = lowpan_recon_start(&frame_address, recon, buf, len);
      } else {
        rv = lowpan_recon_add(recon, buf, len);
      }
        
      if (rv < 0) {
        recon->r_timeout = T_FAILED1;
        goto fail;
      } else {
        // printf("start recon buf: %p\n", recon->r_buf);
        recon->r_timeout = T_ACTIVE;
        recon->r_source_key = source_key;
        recon->r_tag = tag;
      }

      if (recon->r_size == recon->r_bytes_rcvd) {
        deliver(recon);
      }

    } else {
      /* no fragmentation, just deliver it */
      int rv;
      struct lowpan_reconstruct recon;

      /* fill in metadata */
      memcpy(&recon.r_meta.sender, &frame_address.ieee_src, 
             sizeof(ieee154_addr_t));
      recon.r_meta.lqi = call ReadLqi.readLqi(msg);
      recon.r_meta.rssi = call ReadLqi.readRssi(msg);

      buf = getLowpanPayload(&lowmsg);
      if ((rv = lowpan_recon_start(&frame_address, &recon, buf, len)) < 0) {
        goto fail;
      }

      if (recon.r_size == recon.r_bytes_rcvd) {
        deliver(&recon);
      } else {
        // printf("ip_free(%p)\n", recon.r_buf);
        ip_free(recon.r_buf);
      }
    }
    goto done;
  fail:
    BLIP_STATS_INCR(stats.rx_drop);
  done:
    return msg;
  }


  /*
   * Send-side functionality
   */
  task void sendTask() {
    struct send_entry *s_entry;

    // printf("sendTask() - sending\n");

    if (radioBusy || state != S_RUNNING) return;
    if (call SendQueue.empty()) return;
    // this does not dequeue
    s_entry = call SendQueue.head();

#ifdef LPL_SLEEP_INTERVAL
    call LowPowerListening.setRemoteWakeupInterval(s_entry->msg,
            call LowPowerListening.getLocalWakeupInterval());
#endif

    if (s_entry->info->failed) {
      dbg("Drops", "drops: sendTask: dropping failed fragment\n");
      goto fail;
    }

    if ((call Ieee154Send.send(s_entry->msg,
                               call BarePacket.payloadLength(s_entry->msg))) != SUCCESS) {
      dbg("Drops", "drops: sendTask: send failed\n");
      goto fail;
    } else {
      radioBusy = TRUE;
    }

    return;
  fail:
    printf("SEND FAIL\n");
    post sendTask();
    BLIP_STATS_INCR(stats.tx_drop);

    // deallocate the memory associated with this request.
    // other fragments associated with this packet will get dropped.
    s_entry->info->failed = TRUE;
    SENDINFO_DECR(s_entry->info);
    call FragPool.put(s_entry->msg);
    call SendEntryPool.put(s_entry);
    call SendQueue.dequeue();
  }
  

  /*
   *  it will pack the message into the fragment pool and enqueue
   *  those fragments for sending
   *
   * it will set
   *  - payload length
   *  - version, traffic class and flow label
   *
   * the source and destination IP addresses must be set by higher
   * layers.
   */
  command error_t IPLower.send(struct ieee154_frame_addr *frame_addr,
                               struct ip6_packet *msg,
                               void  *data) {
    struct lowpan_ctx ctx;
    struct send_info  *s_info;
    struct send_entry *s_entry;
    message_t *outgoing;

    int frag_len = 1;
    error_t rc = SUCCESS;

    if (state != S_RUNNING) {
      return EOFF;
    }

    /* set version to 6 in case upper layers forgot */
    msg->ip6_hdr.ip6_vfc &= ~IPV6_VERSION_MASK;
    msg->ip6_hdr.ip6_vfc |= IPV6_VERSION;

    ctx.tag = current_local_label++;
    ctx.offset = 0;

    s_info = getSendInfo();
    if (s_info == NULL) {
      rc = ERETRY;
      goto cleanup_outer;
    }
    s_info->upper_data = data;

    while (frag_len > 0) {
      s_entry  = call SendEntryPool.get();
      outgoing = call FragPool.get();

      if (s_entry == NULL || outgoing == NULL) {
        if (s_entry != NULL)
          call SendEntryPool.put(s_entry);
        if (outgoing != NULL)
          call FragPool.put(outgoing);
        // this will cause any fragments we have already enqueued to
        // be dropped by the send task.
        s_info->failed = TRUE;
        printf("drops: IP send: no fragments\n");
        rc = ERETRY;
        goto done;
      }

      call BarePacket.clear(outgoing);
      frag_len = lowpan_frag_get(call Ieee154Send.getPayload(outgoing, 0),
                                 call BarePacket.maxPayloadLength(),
                                 msg,
                                 frame_addr,
                                 &ctx);
      if (frag_len < 0) {
        printf(" get frag error: %i\n", frag_len);
      }

      printf("fragment length: %i offset: %i\n", frag_len, ctx.offset);
      call BarePacket.setPayloadLength(outgoing, frag_len);

      if (frag_len <= 0) {
        call FragPool.put(outgoing);
        call SendEntryPool.put(s_entry);
        goto done;
      }

      if (call SendQueue.enqueue(s_entry) != SUCCESS) {
        BLIP_STATS_INCR(stats.encfail);
        s_info->failed = TRUE;
        printf("drops: IP send: enqueue failed\n");
        goto done;
      }

      s_info->link_fragments++;
      s_entry->msg = outgoing;
      s_entry->info = s_info;

      /* configure the L2 */
      if (frame_addr->ieee_dst.ieee_mode == IEEE154_ADDR_SHORT &&
          frame_addr->ieee_dst.i_saddr == IEEE154_BROADCAST_ADDR) {
        call PacketLink.setRetries(s_entry->msg, 0);
      } else {
        call PacketLink.setRetries(s_entry->msg, BLIP_L2_RETRIES);
      }
      call PacketLink.setRetryDelay(s_entry->msg, BLIP_L2_DELAY);

      SENDINFO_INCR(s_info);}
       
    // printf("got %i frags\n", s_info->link_fragments);
  done:
    BLIP_STATS_INCR(stats.sent);
    SENDINFO_DECR(s_info);
    post sendTask();
  cleanup_outer:
    return rc;
  }

  event void Ieee154Send.sendDone(message_t *msg, error_t error) {
    struct send_entry *s_entry = call SendQueue.head();

    radioBusy = FALSE;

    // printf("sendDone: %p %i\n", msg, error);

    if (state == S_STOPPING) {
      call RadioControl.stop();
      state = S_STOPPED;
      goto done;
    }
    
    s_entry->info->link_transmissions += (call PacketLink.getRetries(msg));
    s_entry->info->link_fragment_attempts++;

    if (!call PacketLink.wasDelivered(msg)) {
      printf("sendDone: was not delivered! (%i tries)\n", 
                 call PacketLink.getRetries(msg));
      s_entry->info->failed = TRUE;
      signal IPLower.sendDone(s_entry->info);
/*       if (s_entry->info->policy.dest[0] != 0xffff) */
/*         dbg("Drops", "drops: sendDone: frag was not delivered\n"); */
      // need to check for broadcast frames
      // BLIP_STATS_INCR(stats.tx_drop);
    } else if (s_entry->info->link_fragment_attempts == 
               s_entry->info->link_fragments) {
      signal IPLower.sendDone(s_entry->info);
    }

  done:
    // kill off any pending fragments
    SENDINFO_DECR(s_entry->info);
    call FragPool.put(s_entry->msg);
    call SendEntryPool.put(s_entry);
    call SendQueue.dequeue();

    post sendTask();
  }

#if 0
  command struct tlv_hdr *IPExtensions.findTlv(struct ip6_ext *ext, uint8_t tlv_val) {
    int len = ext->len - sizeof(struct ip6_ext);
    struct tlv_hdr *tlv = (struct tlv_hdr *)(ext + 1);
    while (len > 0) {
      if (tlv->type == tlv_val) return tlv;
      if (tlv->len == 0) return NULL;
      tlv = (struct tlv_hdr *)(((uint8_t *)tlv) + tlv->len);
      len -= tlv->len;
    }
    return NULL;
  }
#endif


  /*
   * BlipStatistics interface
   */
  command void BlipStatistics.get(ip_statistics_t *statistics) {
#ifdef BLIP_STATS_IP_MEM
    stats.fragpool = call FragPool.size();
    stats.sendinfo = call SendInfoPool.size();
    stats.sendentry= call SendEntryPool.size();
    stats.sndqueue = call SendQueue.size();
    stats.heapfree = ip_malloc_freespace();
    printf("frag: %i sendinfo: %i sendentry: %i sendqueue: %i heap: %i\n",
               stats.fragpool,
               stats.sendinfo,
               stats.sendentry,
               stats.sndqueue,
               stats.heapfree);
#endif
    memcpy(statistics, &stats, sizeof(ip_statistics_t));

  }

  command void BlipStatistics.clear() {
    memclr((uint8_t *)&stats, sizeof(ip_statistics_t));
  }

/*   default event void IP.recv[uint8_t nxt_hdr](struct ip6_hdr *iph, */
/*                                               void *payload, */
/*                                               struct ip_metadata *meta) { */
/*   } */

/*   default event void Multicast.recv[uint8_t scope](struct ip6_hdr *iph, */
/*                                                    void *payload, */
/*                                                    struct ip_metadata *meta) { */
/*   } */
}
