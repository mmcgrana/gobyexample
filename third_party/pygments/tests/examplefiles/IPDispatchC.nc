/*
 * "Copyright (c) 2008-2011 The Regents of the University  of California.
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

/**
 * 
 *
 */
#include "IPDispatch.h"
#include "BlipStatistics.h"

configuration IPDispatchC {
  provides {
    interface SplitControl;
    interface IPLower;
    interface BlipStatistics<ip_statistics_t>;
  }
} implementation {
  
  components MainC;
  components NoLedsC as LedsC;

  /* IPDispatchP wiring -- fragment rassembly and lib6lowpan bindings */
  components IPDispatchP;
  components CC2420RadioC as MessageC;
  components ReadLqiC;
  components new TimerMilliC();

  SplitControl = IPDispatchP.SplitControl;
  IPLower = IPDispatchP;
  BlipStatistics    = IPDispatchP;

  IPDispatchP.Boot -> MainC;
/* #else */
/*   components ResourceSendP; */
/*   ResourceSendP.SubSend -> MessageC; */
/*   ResourceSendP.Resource -> MessageC.SendResource[unique("RADIO_SEND_RESOURCE")]; */
/*   IPDispatchP.Ieee154Send -> ResourceSendP.Ieee154Send; */
/* #endif */
  IPDispatchP.RadioControl -> MessageC;

  IPDispatchP.BarePacket -> MessageC.BarePacket;
  IPDispatchP.Ieee154Send -> MessageC.BareSend;
  IPDispatchP.Ieee154Receive -> MessageC.BareReceive;

#ifdef LOW_POWER_LISTENING
   IPDispatchP.LowPowerListening -> MessageC;
#endif
  MainC.SoftwareInit -> IPDispatchP.Init;

  IPDispatchP.PacketLink -> MessageC;
  IPDispatchP.ReadLqi -> ReadLqiC;
  IPDispatchP.Leds -> LedsC;
  IPDispatchP.ExpireTimer -> TimerMilliC;

  components new PoolC(message_t, N_FRAGMENTS) as FragPool;
  components new PoolC(struct send_entry, N_FRAGMENTS) as SendEntryPool;
  components new QueueC(struct send_entry *, N_FRAGMENTS);
  components new PoolC(struct send_info, N_CONCURRENT_SENDS) as SendInfoPool;
  
  IPDispatchP.FragPool -> FragPool;
  IPDispatchP.SendEntryPool -> SendEntryPool;
  IPDispatchP.SendInfoPool  -> SendInfoPool;
  IPDispatchP.SendQueue -> QueueC;

  components IPNeighborDiscoveryP;
  IPDispatchP.NeighborDiscovery -> IPNeighborDiscoveryP;

/*   components ICMPResponderC; */
/* #ifdef BLIP_MULTICAST */
/*   components MulticastP; */
/*   components new TrickleTimerMilliC(2, 30, 2, 1); */
/*   IP = MulticastP.IP; */

/*   MainC.SoftwareInit -> MulticastP.Init; */
/*   MulticastP.MulticastRx -> IPDispatchP.Multicast; */
/*   MulticastP.HopHeader -> IPExtensionP.HopByHopExt[0]; */
/*   MulticastP.TrickleTimer -> TrickleTimerMilliC.TrickleTimer[0]; */
/*   MulticastP.IPExtensions -> IPDispatchP; */
/* #endif */

#ifdef DELUGE
  components NWProgC;
#endif

}
