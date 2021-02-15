∇ R←M COMBIN N;D;E;F;G;P
  ⍝ Returns a matrix of every possible
  ⍝ combination of M elements from the
  ⍝ vector ⍳N.  That is, returns a
  ⍝ matrix with M!N rows and N columns.
  ⍝
  E←(⍳P←N-R←M-1)-⎕IO
  D←R+⍳P
  R←(P,1)⍴D
  P←P⍴1
 L1:→(⎕IO>1↑D←D-1)⍴0
  P←+\P
  G←+\¯1↓0,F←⌽P
  E←F/E-G
  R←(F/D),R[E+⍳⍴E;]
  E←G
  →L1
∇

∇ R←M QUICKEXP N
  ⍝ Matrix exponentiation
  B ← ⌊ 1 + 2 ⍟ N
  V ← (B ⍴ 2) ⊤ N
  L ← ⊂ M
  R ← ⊃ +.× / V / L ⊣ { L ← (⊂ A +.× A ← ↑L) , L }¨ ⍳ B-1
∇
