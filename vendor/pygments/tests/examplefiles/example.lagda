\documentclass{article}
% this is a LaTeX comment
\usepackage{agda}

\begin{document}

Here's how you can define \emph{RGB} colors in Agda:

\begin{code}
module example where

open import Data.Fin
open import Data.Nat

data Color : Set where
    RGB : Fin 256 → Fin 256 → Fin 256 → Color
\end{code}

\end{document}