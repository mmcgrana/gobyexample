module Main

data Ty = TyInt | TyBool | TyFun Ty Ty

interpTy : Ty -> Type
interpTy TyInt       = Int
interpTy TyBool      = Bool
interpTy (TyFun s t) = interpTy s -> interpTy t

using (G : Vect n Ty)

  data Env : Vect n Ty -> Type where
      Nil  : Env Nil
      (::) : interpTy a -> Env G -> Env (a :: G)

  data HasType : (i : Fin n) -> Vect n Ty -> Ty -> Type where
      stop : HasType fZ (t :: G) t
      pop  : HasType k G t -> HasType (fS k) (u :: G) t

  lookup : HasType i G t -> Env G -> interpTy t
  lookup stop    (x :: xs) = x
  lookup (pop k) (x :: xs) = lookup k xs

  data Expr : Vect n Ty -> Ty -> Type where
      Var : HasType i G t -> Expr G t
      Val : (x : Int) -> Expr G TyInt
      Lam : Expr (a :: G) t -> Expr G (TyFun a t)
      App : Expr G (TyFun a t) -> Expr G a -> Expr G t
      Op  : (interpTy a -> interpTy b -> interpTy c) -> Expr G a -> Expr G b ->
            Expr G c
      If  : Expr G TyBool -> Expr G a -> Expr G a -> Expr G a
      Bind : Expr G a -> (interpTy a -> Expr G b) -> Expr G b

  dsl expr
      lambda      = Lam
      variable    = Var
      index_first = stop
      index_next  = pop

  (<$>) : |(f : Expr G (TyFun a t)) -> Expr G a -> Expr G t
  (<$>) = \f, a => App f a

  pure : Expr G a -> Expr G a
  pure = id

  syntax IF [x] THEN [t] ELSE [e] = If x t e

  (==) : Expr G TyInt -> Expr G TyInt -> Expr G TyBool
  (==) = Op (==)

  (<) : Expr G TyInt -> Expr G TyInt -> Expr G TyBool
  (<) = Op (<)

  instance Num (Expr G TyInt) where
    (+) x y = Op (+) x y
    (-) x y = Op (-) x y
    (*) x y = Op (*) x y

    abs x = IF (x < 0) THEN (-x) ELSE x

    fromInteger = Val . fromInteger

  ||| Evaluates an expression in the given context.
  interp : Env G -> {static} Expr G t -> interpTy t
  interp env (Var i)     = lookup i env
  interp env (Val x)     = x
  interp env (Lam sc)    = \x => interp (x :: env) sc
  interp env (App f s)   = (interp env f) (interp env s)
  interp env (Op op x y) = op (interp env x) (interp env y)
  interp env (If x t e)  = if (interp env x) then (interp env t) else (interp env e)
  interp env (Bind v f)  = interp env (f (interp env v))

  eId : Expr G (TyFun TyInt TyInt)
  eId = expr (\x => x)

  eTEST : Expr G (TyFun TyInt (TyFun TyInt TyInt))
  eTEST = expr (\x, y => y)

  eAdd : Expr G (TyFun TyInt (TyFun TyInt TyInt))
  eAdd = expr (\x, y => Op (+) x y)

  eDouble : Expr G (TyFun TyInt TyInt)
  eDouble = expr (\x => App (App eAdd x) (Var stop))

  eFac : Expr G (TyFun TyInt TyInt)
  eFac = expr (\x => IF x == 0 THEN 1 ELSE [| eFac (x - 1) |] * x)

testFac : Int
testFac = interp [] eFac 4

--testFacTooBig : Int
--testFacTooBig = interp [] eFac 100000

 {-testFacTooBig2 : Int
testFacTooBig2 = interp [] eFac 1000
-}

main : IO ()
main = print testFac


