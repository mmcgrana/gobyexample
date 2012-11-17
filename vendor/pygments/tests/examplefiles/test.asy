// example file for roundedpath() in roundedpath.asy
// written by stefan knorr


// import needed packages
import roundedpath;

// function definition
picture CreateKOOS(real Scale, string legend)               // draw labeled coordinate system as picture
{
  picture ReturnPic;
  real S = 1.2*Scale;
  draw(ReturnPic, ((-S,0)--(S,0)), bar = EndArrow);         // x axis
  draw(ReturnPic, ((0,-S)--(0,S)), bar = EndArrow);         // y axis
  label(ReturnPic, "$\varepsilon$", (S,0), SW);             // x axis label
  label(ReturnPic, "$\sigma$", (0,S), SW);                  // y axis label
  label(ReturnPic, legend, (0.7S, -S), NW);                 // add label 'legend' 
  return ReturnPic;                                         // return picture
}


// some global definitions
real S = 13mm;                          // universal scale factor for the whole file
real grad = 0.25;                       // gradient for lines
real radius = 0.04;                     // radius for the rounded path'
real lw = 2;                            // linewidth
pair A = (-1, -1);                      // start point for graphs
pair E = ( 1,  1);                      // end point for graphs
path graph;                             // local graph
pen ActPen;                             // actual pen for each drawing
picture T[];                            // vector of all four diagrams
real inc = 2.8;                         // increment-offset for combining pictures

//////////////////////////////////////// 1st diagram
T[1] = CreateKOOS(S, "$T_1$");                                        // initialise T[1] as empty diagram with label $T_1$                  
graph = A;                                                            //  # pointwise definition of current path 'graph'                    
graph = graph -- (A.x + grad*1.6, A.y + 1.6);                         //  #                                                                 
graph = graph -- (E.x - grad*0.4, E.y - 0.4);                         //  #                                                                 
graph = graph -- E;                                                   //  #

graph = roundedpath(graph, radius, S);                                // round edges of 'graph' using roundedpath() in roundedpath.asy      
ActPen =  rgb(0,0,0.6) + linewidth(lw);                               // define pen for drawing in 1st diagram                              
draw(T[1],                   graph, ActPen);                          // draw 'graph' with 'ActPen' into 'T[1]' (1st hysteresis branch)     
draw(T[1], rotate(180,(0,0))*graph, ActPen);                          // draw rotated 'graph' (2nd hysteresis branch)                       

graph = (0,0) -- (grad*0.6, 0.6) -- ( (grad*0.6, 0.6) + (0.1, 0) );   // define branch from origin to hysteresis
graph = roundedpath(graph, radius, S);                                // round this path                        
draw(T[1], graph, ActPen);                                            // draw this path into 'T[1]'             


//////////////////////////////////////// 2nd diagram
T[2] = CreateKOOS(S, "$T_2$");                                        // initialise T[2] as empty diagram with label $T_2$              
graph = A;                                                            //  # pointwise definition of current path 'graph'                
graph = graph -- (A.x + grad*1.3, A.y + 1.3);                         //  #                                                             
graph = graph -- (E.x - grad*0.7 , E.y - 0.7);                        //  #                                                             
graph = graph -- E;                                                   //  #
                                                                                                
graph = roundedpath(graph, radius, S);                                // round edges of 'graph' using roundedpath() in roundedpath.asy      
ActPen =  rgb(0.2,0,0.4) + linewidth(lw);                             // define pen for drawing in 2nd diagram                              
draw(T[2],                   graph, ActPen);                          // draw 'graph' with 'ActPen' into 'T[2]' (1st hysteresis branch)     
draw(T[2], rotate(180,(0,0))*graph, ActPen);                          // draw rotated 'graph' (2nd hysteresis branch)                       

graph = (0,0) -- (grad*0.3, 0.3) -- ( (grad*0.3, 0.3) + (0.1, 0) );   // define branch from origin to hysteresis
graph = roundedpath(graph, radius, S);                                // round this path                        
draw(T[2], graph, ActPen);                                            // draw this path into 'T[2]'             


//////////////////////////////////////// 3rd diagram
T[3] = CreateKOOS(S, "$T_3$");                                        // initialise T[3] as empty diagram with label $T_3$              
graph = A;                                                            //  # pointwise definition of current path 'graph'                
graph = graph -- (A.x + grad*0.7, A.y + 0.7);                         //  #                                                             
graph = graph -- ( - grad*0.3 , - 0.3);                               //  #                                                             
graph = graph -- (0,0);                                               //  #                                                             
graph = graph -- (grad*0.6, 0.6);                                     //  #                                                             
graph = graph -- (E.x - grad*0.4, E.y - 0.4);                         //  #                                                             
graph = graph -- E;                                                   //  #                                                             
                                                                                                                                        
graph = roundedpath(graph, radius, S);                                // round edges of 'graph' using roundedpath() in roundedpath.asy  
ActPen =  rgb(0.6,0,0.2) + linewidth(lw);                             // define pen for drawing in 3rd diagram                          
draw(T[3],                   graph, ActPen);                          // draw 'graph' with 'ActPen' into 'T[3]' (1st hysteresis branch) 
draw(T[3], rotate(180,(0,0))*graph, ActPen);                          // draw rotated 'graph' (2nd hysteresis branch)                   


//////////////////////////////////////// 4th diagram
T[4] = CreateKOOS(S, "$T_4$");                                        // initialise T[4] as empty diagram with label $T_4$              
graph = A;                                                            //  # pointwise definition of current path 'graph'                
graph = graph -- (A.x + grad*0.4, A.y + 0.4);                         //  #                                                             
graph = graph -- ( - grad*0.6 , - 0.6);                               //  #                                                             
graph = graph -- (0,0);                                               //  #                                                             
graph = graph -- (grad*0.9, 0.9);                                     //  #                                                             
graph = graph -- (E.x - grad*0.1, E.y - 0.1);                         //  #                                                             
graph = graph -- E;                                                   //  #                                                             
                                                                                                                                        
graph = roundedpath(graph, radius, S);                                // round edges of 'graph' using roundedpath() in roundedpath.asy  
ActPen =  rgb(0.6,0,0) + linewidth(lw);                               // define pen for drawing in 4th diagram                          
draw(T[4],                   graph, ActPen);                          // draw 'graph' with 'ActPen' into 'T[4]' (1st hysteresis branch) 
draw(T[4], rotate(180,(0,0))*graph, ActPen);                          // draw rotated 'graph' (3nd hysteresis branch)                   


// add some labels and black dots to the first two pictures
pair SWW = (-0.8, -0.6);
label(T[1], "$\sigma_f$", (0, 0.6S), NE);                             // sigma_f
draw(T[1], (0, 0.6S), linewidth(3) + black);
label(T[2], "$\sigma_f$", (0, 0.3S), NE);                             // sigma_f
draw(T[2], (0, 0.3S), linewidth(3) + black);
label(T[1], "$\varepsilon_p$", (0.7S, 0), SWW);                       // epsilon_p
draw(T[1], (0.75S, 0), linewidth(3) + black);
label(T[2], "$\varepsilon_p$", (0.7S, 0), SWW);                       // epsilon_p
draw(T[2], (0.75S, 0), linewidth(3) + black);


// add all pictures T[1...4] to the current one
add(T[1],(0,0));
add(T[2],(1*inc*S,0));
add(T[3],(2*inc*S,0));
add(T[4],(3*inc*S,0));


// draw line of constant \sigma and all intersection points with the graphs in T[1...4]
ActPen = linewidth(1) + dashed + gray(0.5);                           // pen definition
draw((-S, 0.45*S)--((3*inc+1)*S, 0.45*S), ActPen);                    // draw backgoundline
label("$\sigma_s$", (-S, 0.45S), W);                            // label 'sigma_s'

path mark = scale(2)*unitcircle;                                  // define mark-symbol to be used for intersections
ActPen = linewidth(1) + gray(0.5);                                    // define pen for intersection mark
draw(shift(( 1 - grad*0.55 + 0*inc)*S, 0.45*S)*mark, ActPen);         //  # draw all intersections
draw(shift((-1 + grad*1.45 + 0*inc)*S, 0.45*S)*mark, ActPen);         //  #
draw(shift(( 1 - grad*0.55 + 1*inc)*S, 0.45*S)*mark, ActPen);         //  #
draw(shift(( 1 - grad*0.55 + 2*inc)*S, 0.45*S)*mark, ActPen);         //  # 
draw(shift((     grad*0.45 + 2*inc)*S, 0.45*S)*mark, ActPen);         //  # 
draw(shift((     grad*0.45 + 3*inc)*S, 0.45*S)*mark, ActPen);         //  #
