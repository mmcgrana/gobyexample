module toplevel(clock,reset);
 input clock;
 input reset;
 
 reg flop1;
 reg flop2;
 
 always @ (posedge reset or posedge clock)
 if (reset)
   begin
     flop1 <= 0;
     flop2 <= 1;
   end
 else
   begin
     flop1 <= flop2;
     flop2 <= flop1;
   end
endmodule
