module: nanomsg
synopsis: generated bindings for the nanomsg library
author: Bruce Mitchener, Jr.
copyright: See LICENSE file in this distribution.

define simple-C-mapped-subtype <C-buffer-offset> (<C-char*>)
  export-map <machine-word>, export-function: identity;
end;

define interface
  #include {
      "sp/sp.h",
      "sp/fanin.h",
      "sp/inproc.h",
      "sp/pair.h",
      "sp/reqrep.h",
      "sp/survey.h",
      "sp/fanout.h",
      "sp/ipc.h",
      "sp/pubsub.h",
      "sp/tcp.h"
    },

    exclude: {
      "SP_HAUSNUMERO",
      "SP_PAIR_ID",
      "SP_PUBSUB_ID",
      "SP_REQREP_ID",
      "SP_FANIN_ID",
      "SP_FANOUT_ID",
      "SP_SURVEY_ID"
    },

    equate: {"char *" => <c-string>},

    rename: {
      "sp_recv" => %sp-recv,
      "sp_send" => %sp-send,
      "sp_setsockopt" => %sp-setsockopt
    };

    function "sp_version",
      output-argument: 1,
      output-argument: 2,
      output-argument: 3;

    function "sp_send",
      map-argument: { 2 => <C-buffer-offset> };

    function "sp_recv",
      map-argument: { 2 => <C-buffer-offset> };

end interface;

// Function for adding the base address of the repeated slots of a <buffer>
// to an offset and returning the result as a <machine-word>.  This is
// necessary for passing <buffer> contents across the FFI.

define function buffer-offset
    (the-buffer :: <buffer>, data-offset :: <integer>)
 => (result-offset :: <machine-word>)
  u%+(data-offset,
      primitive-wrap-machine-word
        (primitive-repeated-slot-as-raw
           (the-buffer, primitive-repeated-slot-offset(the-buffer))))
end function;

define inline function sp-send (socket :: <integer>, data :: <buffer>, flags :: <integer>) => (res :: <integer>)
  %sp-send(socket, buffer-offset(data, 0), data.size, flags)
end;

define inline function sp-recv (socket :: <integer>, data :: <buffer>, flags :: <integer>) => (res :: <integer>)
  %sp-recv(socket, buffer-offset(data, 0), data.size, flags);
end;

define inline method sp-setsockopt (socket :: <integer>, level :: <integer>, option :: <integer>, value :: <integer>)
  with-stack-structure (int :: <C-int*>)
    pointer-value(int) := value;
    let setsockopt-result =
      %sp-setsockopt(socket, level, option, int, size-of(<C-int*>));
    if (setsockopt-result < 0)
      // Check error!
    end;
    setsockopt-result
  end;
end;

define inline method sp-setsockopt (socket :: <integer>, level :: <integer>, option :: <integer>, data :: <byte-string>)
  let setsockopt-result =
    %sp-setsockopt(socket, level, option, as(<c-string>, data), data.size);
  if (setsockopt-result < 0)
    // Check error!
  end;
  setsockopt-result
end;
