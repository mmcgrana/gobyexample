if (this.running)
  player:tell("[Train] Error: already a jump in progress");
  return;
endif
this.running = 1;
this.aborted = 0;
this:announce_all("[Train] departure in 20 seconds");
dest = this.targets[random(length(this.targets))];
this:announce_all("[Train] Next stop is '", dest:title(), "'");
this:announce_all("You hear the engines starting up");
this.location:announce("The MOOTrain starts up his engines");
suspend(20);
if (this.aborted)
  this.running = 0;
  this.aborted = 0;
  return;
endif
this:announce_all("[Train] Departure!");
this.location:announce_all("The MOOTrain leaves into the 42th dimension!");
this:announce_all("Outside you see the lights of the 42th dimension");
this:moveto(dest);
suspend(4);
this:announce_all("The glowing gets less, until you can see the clear shape of the room, the MOOTrain has landed in");
this.location:announce_all("The MOOTrain arrives out of the 42th dimension!");
this:announce_all("[Train] arrived in '", dest:title(), "'");
this.running = 0;