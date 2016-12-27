(package pygments-test [some symbols]

\* multiline
   comment
*\

\\ With vars as functions

(define super
  [Value Succ End] Action Combine Zero ->
  (if (End Value)
      Zero
      (Combine (Action Value)
               (super [(Succ Value) Succ End]
                      Action Combine Zero))))

(define for
  Stream Action -> (super Stream Action (function do) 0))

(define filter
  Stream Condition ->
  (super Stream
         (/. Val (if (Condition Val) [Val] []))
         (function append)
         []))

(for [0 (+ 1) (= 10)] (function print))

(filter [0 (+ 1) (= 100)]
        (/. X (integer? (/ X 3))))


\\ Typed functions

(define typed-map
  { (A --> B) --> (list A) --> (list B) }
  F X -> (typed-map-h F X []))

(define typed-map-h
  { (A --> B) --> (list A) --> (list B) \\ comment
       --> (list B) }
  _ [] X -> (reverse X)
  F [X | Y] Z -> (typed-map-h F Y [(F X) | Z]))

(define append-string
  { string --> string \* comment *\ --> string }
  S1 S2 -> (cn S1 S2))

(let X 1
     Y 2
  (+ (type X number) (type Y number)))

\\ Yacc

(defcc <st_input>
  <lrb>  <st_input1> <rrb> <st_input2> 
   := (package-macro (macroexpand <st_input1>) <st_input2>);
  <lcurly> <st_input> := [{ | <st_input>];
  <rcurly> <st_input> := [} | <st_input>];    
  <bar> <st_input> := [bar! | <st_input>];  
  <semicolon> <st_input> := [; | <st_input>];
  <colon> <equal> <st_input> := [:= | <st_input>];
  <colon> <minus> <st_input> := [:- | <st_input>];
  <colon> <st_input> := [: | <st_input>];
  <comma> <st_input> := [(intern ",") | <st_input>];
  <e> := [];)
  
(defcc <lsb>
   91 := skip;)

\\ Pattern matching

(define matches
  1 X 3 -> X
  X Y Z -> Y where  (and (= X 1) (= Z 3))
  true false _ -> true
  (@p a X c) (@s X "abc") (@v 1 2 3 <>) -> true
  [X | Rest] [] [a b c] -> true
  [(@p a b)] [[[1] 2] X] "string" -> true
  _ _ _ -> false)


\\ Prolog

(defprolog th*
  X A Hyps <-- (show [X : A] Hyps) (when false);
  X A _ <-- (fwhen (typedf? X)) (bind F (sigf X)) (call [F A]);
  (mode [F] -) A Hyp <-- (th* F [--> A] Hyp);
  (mode [cons X Y] -) [list A] Hyp <-- (th* X A Hyp) (th* Y [list A] Hyp);
  (mode [@s X Y] -) string Hyp <-- (th* X string Hyp) (th* Y string Hyp);
  (mode [lambda X Y] -) [A --> B] Hyp <-- ! 
                                           (bind X&& (placeholder)) 
                                           (bind Z (ebr X&& X Y))
                                           (th* Z B [[X&& : A] | Hyp]); 
  (mode [type X A] -) B Hyp <-- ! (unify A B) (th* X A Hyp);)

\\ Macros

(defmacro log-macro
  [log N] -> [log N 10])

\\ Sequent calculus

(datatype rank

  if (element? X [ace 2 3 4 5 6 7 8 9 10 jack queen king])
  ________
  X : rank;)

(datatype suit

  if (element? Suit [spades hearts diamonds clubs])
  _________
  Suit : suit;)

(datatype card

  Rank : rank; Suit : suit;
  _________________
  [Rank Suit] : card;

  Rank : rank, Suit : suit >> P;
  _____________________
  [Rank Suit] : card >> P;)

(datatype card

  Rank : rank; Suit : suit;
  ==================
  [Rank Suit] : card;)

\\ String interpolation and escape sequences

"abc~A ~S~R ~% blah
 c#30;c#31;blah"

)
