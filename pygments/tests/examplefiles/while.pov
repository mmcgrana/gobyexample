#declare Index1 = 0;
#while(Index1 <= 9)

   #declare Index2 = 0;
   #while(Index2 <= 19)

      sphere { <Index1, Index2, 0>, .5 }

      #declare Index2 = Index2 + 1;
   #end

   #declare Index1 = Index1 + 1;
#end
