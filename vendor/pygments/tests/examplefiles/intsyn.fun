(* Internal Syntax *)
(* Author: Frank Pfenning, Carsten Schuermann *)
(* Modified: Roberto Virga *)

functor IntSyn (structure Global : GLOBAL) :> INTSYN =
struct

  type cid = int			(* Constant identifier        *)
  type name = string			(* Variable name              *)
  type mid = int                        (* Structure identifier       *)
  type csid = int                       (* CS module identifier       *)


  (* Contexts *)
  datatype 'a Ctx =			(* Contexts                   *)
    Null				(* G ::= .                    *)
  | Decl of 'a Ctx * 'a			(*     | G, D                 *)

  (* ctxPop (G) => G'
     Invariant: G = G',D
  *)
  fun ctxPop (Decl (G, D)) = G

  exception Error of string             (* raised if out of space     *) 
  (* ctxLookup (G, k) = D, kth declaration in G from right to left
     Invariant: 1 <= k <= |G|, where |G| is length of G
  *)

  fun ctxLookup (Decl (G', D), 1) = D
    | ctxLookup (Decl (G', _), k') = ctxLookup (G', k'-1)
(*    | ctxLookup (Null, k') = (print ("Looking up k' = " ^ Int.toString k' ^ "\n"); raise Error "Out of Bounce\n")*)
    (* ctxLookup (Null, k')  should not occur by invariant *)

  (* ctxLength G = |G|, the number of declarations in G *)
  fun ctxLength G =
      let 
	fun ctxLength' (Null, n) = n
	  | ctxLength' (Decl(G, _), n)= ctxLength' (G, n+1)
      in
	ctxLength' (G, 0)
      end
    
  type FgnExp = exn                     (* foreign expression representation *)
  exception UnexpectedFgnExp of FgnExp
                                        (* raised by a constraint solver
					   if passed an incorrect arg *)

  type FgnCnstr = exn                   (* foreign unification constraint
                                           representation *)
  exception UnexpectedFgnCnstr of FgnCnstr
                                        (* raised by a constraint solver
                                           if passed an incorrect arg *)

  datatype Depend =                     (* Dependency information     *)
    No                                  (* P ::= No                   *)
  | Maybe                               (*     | Maybe                *)
  | Meta				(*     | Meta                 *)

  (* Expressions *)

  datatype Uni =			(* Universes:                 *)
    Kind				(* L ::= Kind                 *)
  | Type				(*     | Type                 *)

  datatype Exp =			(* Expressions:               *)
    Uni   of Uni			(* U ::= L                    *)
  | Pi    of (Dec * Depend) * Exp       (*     | bPi (D, P). V         *)
  | Root  of Head * Spine		(*     | C @ S                *)
  | Redex of Exp * Spine		(*     | U @ S                *)
  | Lam   of Dec * Exp			(*     | lam D. U             *)
  | EVar  of Exp option ref * Dec Ctx * Exp * (Cnstr ref) list ref
                                        (*     | X<I> : G|-V, Cnstr   *)

  | EClo  of Exp * Sub			(*     | U[s]                 *)
  | AVar  of Exp option ref             (*     | A<I>                 *)   
  | NVar  of int			(*     | n (linear, fully applied) *)
                                        (* grafting variable *)

  | FgnExp of csid * FgnExp
                                        (*     | (foreign expression) *)
    
  and Head =				(* Heads:                     *)
    BVar  of int			(* H ::= k                    *)
  | Const of cid			(*     | c                    *)
  | Proj  of Block * int		(*     | #k(b)                *)
  | Skonst of cid			(*     | c#                   *)
  | Def   of cid			(*     | d                    *)
  | NSDef of cid			(*     | d (non strict)       *)
  | FVar  of name * Exp * Sub		(*     | F[s]                 *)
  | FgnConst of csid * ConDec           (*     | (foreign constant)   *)
    
  and Spine =				(* Spines:                    *)
    Nil					(* S ::= Nil                  *)
  | App   of Exp * Spine		(*     | U ; S                *)
  | SClo  of Spine * Sub		(*     | S[s]                 *)

  and Sub =				(* Explicit substitutions:    *)
    Shift of int			(* s ::= ^n                   *)
  | Dot   of Front * Sub		(*     | Ft.s                 *)

  and Front =				(* Fronts:                    *)
    Idx of int				(* Ft ::= k                   *)
  | Exp of Exp				(*     | U                    *)
  | Axp of Exp				(*     | U (assignable)       *)
  | Block of Block			(*     | _x                   *)
  | Undef				(*     | _                    *)

  and Dec =				(* Declarations:              *)
    Dec of name option * Exp		(* D ::= x:V                  *)
  | BDec of name option * (cid * Sub)	(*     | v:l[s]               *)
  | ADec of name option * int   	(*     | v[^-d]               *)
  | NDec of name option

  and Block =				(* Blocks:                    *)
    Bidx of int 			(* b ::= v                    *)
  | LVar of Block option ref * Sub * (cid * Sub)
                                        (*     | L(l[^k],t)           *)
  | Inst of Exp list			(*     | u1, ..., Un          *)


  (* Constraints *)

  and Cnstr =				(* Constraint:                *)
    Solved                      	(* Cnstr ::= solved           *)
  | Eqn      of Dec Ctx * Exp * Exp     (*         | G|-(U1 == U2)    *)
  | FgnCnstr of csid * FgnCnstr         (*         | (foreign)        *)

  and Status =                          (* Status of a constant:      *)
    Normal                              (*   inert                    *)
  | Constraint of csid * (Dec Ctx * Spine * int -> Exp option)
                                        (*   acts as constraint       *)
  | Foreign of csid * (Spine -> Exp)    (*   is converted to foreign  *)

  and FgnUnify =                        (* Result of foreign unify    *)
    Succeed of FgnUnifyResidual list
    (* succeed with a list of residual operations *)
  | Fail

  and FgnUnifyResidual =                (* Residual of foreign unify  *)
    Assign of Dec Ctx * Exp * Exp * Sub
    (* perform the assignment G |- X = U [ss] *)
  | Delay of Exp * Cnstr ref
    (* delay cnstr, associating it with all the rigid EVars in U  *)

  (* Global signature *)

  and ConDec =			        (* Constant declaration       *)
    ConDec of string * mid option * int * Status
                                        (* a : K : kind  or           *)
              * Exp * Uni	        (* c : A : type               *)
  | ConDef of string * mid option * int	(* a = A : K : kind  or       *)
              * Exp * Exp * Uni		(* d = M : A : type           *)
              * Ancestor                (* Ancestor info for d or a   *)
  | AbbrevDef of string * mid option * int
                                        (* a = A : K : kind  or       *)
              * Exp * Exp * Uni		(* d = M : A : type           *)
  | BlockDec of string * mid option     (* %block l : SOME G1 PI G2   *)
              * Dec Ctx * Dec list

  | BlockDef of string * mid option * cid list
                                        (* %block l = (l1 | ... | ln) *)

  | SkoDec of string * mid option * int	(* sa: K : kind  or           *)
              * Exp * Uni	        (* sc: A : type               *)

  and Ancestor =			(* Ancestor of d or a         *)
    Anc of cid option * int * cid option (* head(expand(d)), height, head(expand[height](d)) *)
                                        (* NONE means expands to {x:A}B *)

  datatype StrDec =                     (* Structure declaration      *)
      StrDec of string * mid option

  (* Form of constant declaration *)
  datatype ConDecForm =
    FromCS				(* from constraint domain *)
  | Ordinary				(* ordinary declaration *)
  | Clause				(* %clause declaration *)

  (* Type abbreviations *)
  type dctx = Dec Ctx			(* G = . | G,D                *)
  type eclo = Exp * Sub   		(* Us = U[s]                  *)
  type bclo = Block * Sub   		(* Bs = B[s]                  *)
  type cnstr = Cnstr ref

(*  exception Error of string             (* raised if out of space     *) *)


  structure FgnExpStd = struct

    structure ToInternal = FgnOpnTable (type arg = unit
					type result = Exp)

    structure Map = FgnOpnTable (type arg = Exp -> Exp
				 type result = Exp)

    structure App = FgnOpnTable (type arg = Exp -> unit
				 type result = unit)

    structure EqualTo = FgnOpnTable (type arg = Exp
				     type result = bool)

    structure UnifyWith = FgnOpnTable (type arg = Dec Ctx * Exp
				       type result = FgnUnify)

			  

    fun fold csfe f b = let
	val r = ref b
	fun g U = r := f (U,!r)
    in
	App.apply csfe g ; !r
    end

  end

  structure FgnCnstrStd = struct

    structure ToInternal = FgnOpnTable (type arg = unit
					type result = (Dec Ctx * Exp) list)

    structure Awake = FgnOpnTable (type arg = unit
				   type result = bool)

    structure Simplify = FgnOpnTable (type arg = unit
				      type result = bool)

  end

  fun conDecName (ConDec (name, _, _, _, _, _)) = name
    | conDecName (ConDef (name, _, _, _, _, _, _)) = name
    | conDecName (AbbrevDef (name, _, _, _, _, _)) = name
    | conDecName (SkoDec (name, _, _, _, _)) = name
    | conDecName (BlockDec (name, _, _, _)) = name
    | conDecName (BlockDef (name, _, _)) = name

  fun conDecParent (ConDec (_, parent, _, _, _, _)) = parent
    | conDecParent (ConDef (_, parent, _, _, _, _, _)) = parent
    | conDecParent (AbbrevDef (_, parent, _, _, _, _)) = parent
    | conDecParent (SkoDec (_, parent, _, _, _)) = parent
    | conDecParent (BlockDec (_, parent, _, _)) = parent
    | conDecParent (BlockDef (_, parent, _)) = parent
   

  (* conDecImp (CD) = k

     Invariant:
     If   CD is either a declaration, definition, abbreviation, or 
          a Skolem constant
     then k stands for the number of implicit elements.
  *)
  fun conDecImp (ConDec (_, _, i, _, _, _)) = i
    | conDecImp (ConDef (_, _, i, _, _, _, _)) = i
    | conDecImp (AbbrevDef (_, _, i, _, _, _)) = i
    | conDecImp (SkoDec (_, _, i, _, _)) = i
    | conDecImp (BlockDec (_, _,  _, _)) = 0   (* watch out -- carsten *)

  fun conDecStatus (ConDec (_, _, _, status, _, _)) = status
    | conDecStatus _ = Normal

  (* conDecType (CD) =  V

     Invariant:
     If   CD is either a declaration, definition, abbreviation, or 
          a Skolem constant
     then V is the respective type
  *)
  fun conDecType (ConDec (_, _, _, _, V, _)) = V
    | conDecType (ConDef (_, _, _, _, V, _, _)) = V
    | conDecType (AbbrevDef (_, _, _, _, V, _)) = V
    | conDecType (SkoDec (_, _, _, V, _)) = V


  (* conDecBlock (CD) =  (Gsome, Lpi)

     Invariant:
     If   CD is block definition
     then Gsome is the context of some variables
     and  Lpi is the list of pi variables
  *)
  fun conDecBlock (BlockDec (_, _, Gsome, Lpi)) = (Gsome, Lpi)

  (* conDecUni (CD) =  L

     Invariant:
     If   CD is either a declaration, definition, abbreviation, or 
          a Skolem constant
     then L is the respective universe
  *)
  fun conDecUni (ConDec (_, _, _, _, _, L)) = L
    | conDecUni (ConDef (_, _, _, _, _, L, _)) = L
    | conDecUni (AbbrevDef (_, _, _, _, _, L)) = L
    | conDecUni (SkoDec (_, _, _, _, L)) = L


  fun strDecName (StrDec (name, _)) = name

  fun strDecParent (StrDec (_, parent)) = parent

  local
    val maxCid = Global.maxCid
    val dummyEntry = ConDec("", NONE, 0, Normal, Uni (Kind), Kind)
    val sgnArray = Array.array (maxCid+1, dummyEntry)
      : ConDec Array.array
    val nextCid  = ref(0)

    val maxMid = Global.maxMid
    val sgnStructArray = Array.array (maxMid+1, StrDec("", NONE))
      : StrDec Array.array
    val nextMid = ref (0)

  in
    (* Invariants *)
    (* Constant declarations are all well-typed *)
    (* Constant declarations are stored in beta-normal form *)
    (* All definitions are strict in all their arguments *)
    (* If Const(cid) is valid, then sgnArray(cid) = ConDec _ *)
    (* If Def(cid) is valid, then sgnArray(cid) = ConDef _ *)

    fun sgnClean (i) = if i >= !nextCid then ()
                       else (Array.update (sgnArray, i, dummyEntry);
			     sgnClean (i+1))

    fun sgnReset () = ((* Fri Dec 20 12:04:24 2002 -fp *)
		       (* this circumvents a space leak *)
		       sgnClean (0);
		       nextCid := 0; nextMid := 0)
    fun sgnSize () = (!nextCid, !nextMid)

    fun sgnAdd (conDec) = 
        let
	  val cid = !nextCid
	in
	  if cid > maxCid
	    then raise Error ("Global signature size " ^ Int.toString (maxCid+1) ^ " exceeded")
	  else (Array.update (sgnArray, cid, conDec) ;
		nextCid := cid + 1;
		cid)
	end

    (* 0 <= cid < !nextCid *)
    fun sgnLookup (cid) = Array.sub (sgnArray, cid)

    fun sgnApp (f) =
        let
	  fun sgnApp' (cid) = 
	      if cid = !nextCid then () else (f cid; sgnApp' (cid+1)) 
	in
	  sgnApp' (0)
	end

    fun sgnStructAdd (strDec) = 
        let
	  val mid = !nextMid
	in
	  if mid > maxMid
	    then raise Error ("Global signature size " ^ Int.toString (maxMid+1) ^ " exceeded")
	  else (Array.update (sgnStructArray, mid, strDec) ;
		nextMid := mid + 1;
		mid)
	end

    (* 0 <= mid < !nextMid *)
    fun sgnStructLookup (mid) = Array.sub (sgnStructArray, mid)

    (* A hack used in Flit - jcreed 6/05 *)
    fun rename (cid, new) =
	let
	    val newConDec = case sgnLookup cid of 
		ConDec (n,m,i,s,e,u) => ConDec(new,m,i,s,e,u)
	      | ConDef (n,m,i,e,e',u,a) => ConDef(new,m,i,e,e',u,a)
	      | AbbrevDef (n,m,i,e,e',u) => AbbrevDef (new,m,i,e,e',u)
	      | BlockDec (n,m,d,d') => BlockDec (new,m,d,d')
	      | SkoDec (n,m,i,e,u) => SkoDec (new,m,i,e,u)
	in
	    Array.update (sgnArray, cid, newConDec)
	end

  end

  fun constDef (d) =
      (case sgnLookup (d)
	 of ConDef(_, _, _, U,_, _, _) => U
	  | AbbrevDef (_, _, _, U,_, _) => U)

  fun constType (c) = conDecType (sgnLookup c)
  fun constImp (c) = conDecImp (sgnLookup c)
  fun constUni (c) = conDecUni (sgnLookup c)
  fun constBlock (c) = conDecBlock (sgnLookup c)

  fun constStatus (c) =
      (case sgnLookup (c)
	 of ConDec (_, _, _, status, _, _) => status
          | _ => Normal)


  (* Explicit Substitutions *)

  (* id = ^0 
  
     Invariant:
     G |- id : G        id is patsub
  *)
  val id = Shift(0)

  (* shift = ^1
  
     Invariant:
     G, V |- ^ : G       ^ is patsub
  *)
  val shift = Shift(1)

  (* invShift = ^-1 = _.^0
     Invariant:
     G |- ^-1 : G, V     ^-1 is patsub
  *)
  val invShift = Dot(Undef, id)


  (* comp (s1, s2) = s'

     Invariant:
     If   G'  |- s1 : G 
     and  G'' |- s2 : G'
     then s'  = s1 o s2
     and  G'' |- s1 o s2 : G

     If  s1, s2 patsub
     then s' patsub
   *)
  fun comp (Shift (0), s) = s
    (* next line is an optimization *)
    (* roughly 15% on standard suite for Twelf 1.1 *)
    (* Sat Feb 14 10:15:16 1998 -fp *)
    | comp (s, Shift (0)) = s
    | comp (Shift (n), Dot (Ft, s)) = comp (Shift (n-1), s)
    | comp (Shift (n), Shift (m)) = Shift (n+m)
    | comp (Dot (Ft, s), s') = Dot (frontSub (Ft, s'), comp (s, s'))

  (* bvarSub (n, s) = Ft'
   
      Invariant: 
     If    G |- s : G'    G' |- n : V
     then  Ft' = Ftn         if  s = Ft1 .. Ftn .. ^k
       or  Ft' = ^(n+k)     if  s = Ft1 .. Ftm ^k   and m<n
     and   G |- Ft' : V [s]
  *)
  and bvarSub (1, Dot(Ft, s)) = Ft
    | bvarSub (n, Dot(Ft, s)) = bvarSub (n-1, s)
    | bvarSub (n, Shift(k))  = Idx (n+k)

  (* blockSub (B, s) = B' 
    
     Invariant:
     If   G |- s : G'   
     and  G' |- B block
     then G |- B' block
     and  B [s] == B' 
  *)
  (* in front of substitutions, first case is irrelevant *)
  (* Sun Dec  2 11:56:41 2001 -fp *)
  and blockSub (Bidx k, s) =
      (case bvarSub (k, s)
	 of Idx k' => Bidx k'
          | Block B => B)
    | blockSub (LVar (ref (SOME B), sk, _), s) =
        blockSub (B, comp (sk, s))
    (* -fp Sun Dec  1 21:18:30 2002 *)
    (* --cs Sun Dec  1 11:25:41 2002 *)
    (* Since always . |- t : Gsome, discard s *)
    (* where is this needed? *)
    (* Thu Dec  6 20:30:26 2001 -fp !!! *)
    | blockSub (LVar (r as ref NONE, sk, (l, t)), s) = 
        LVar(r, comp(sk, s), (l, t))
      (* was:
	LVar (r, comp(sk, s), (l, comp (t, s)))
	July 22, 2010 -fp -cs
       *)
	(* comp(^k, s) = ^k' for some k' by invariant *)
    | blockSub (L as Inst ULs, s') = Inst (map (fn U => EClo (U, s')) ULs)
    (* this should be right but somebody should verify *) 

  (* frontSub (Ft, s) = Ft'

     Invariant:
     If   G |- s : G'     G' |- Ft : V
     then Ft' = Ft [s]
     and  G |- Ft' : V [s]

     NOTE: EClo (U, s) might be undefined, so if this is ever
     computed eagerly, we must introduce an "Undefined" exception,
     raise it in whnf and handle it here so Exp (EClo (U, s)) => Undef
  *)
  and frontSub (Idx (n), s) = bvarSub (n, s)
    | frontSub (Exp (U), s) = Exp (EClo (U, s))
    | frontSub (Undef, s) = Undef
    | frontSub (Block (B), s) = Block (blockSub (B, s))

  (* decSub (x:V, s) = D'

     Invariant:
     If   G  |- s : G'    G' |- V : L
     then D' = x:V[s]
     and  G  |- V[s] : L
  *)
  (* First line is an optimization suggested by cs *)
  (* D[id] = D *)
  (* Sat Feb 14 18:37:44 1998 -fp *)
  (* seems to have no statistically significant effect *)
  (* undo for now Sat Feb 14 20:22:29 1998 -fp *)
  (*
  fun decSub (D, Shift(0)) = D
    | decSub (Dec (x, V), s) = Dec (x, EClo (V, s))
  *)
  fun decSub (Dec (x, V), s) = Dec (x, EClo (V, s))
    | decSub (NDec x, s) = NDec x
    | decSub (BDec (n, (l, t)), s) = BDec (n, (l, comp (t, s)))

  (* dot1 (s) = s'

     Invariant:
     If   G |- s : G'
     then s' = 1. (s o ^)
     and  for all V s.t.  G' |- V : L
          G, V[s] |- s' : G', V 

     If s patsub then s' patsub
  *)
  (* first line is an optimization *)
  (* roughly 15% on standard suite for Twelf 1.1 *)
  (* Sat Feb 14 10:16:16 1998 -fp *)
  fun dot1 (s as Shift (0)) = s
    | dot1 s = Dot (Idx(1), comp(s, shift))

  (* invDot1 (s) = s'
     invDot1 (1. s' o ^) = s'

     Invariant:
     s = 1 . s' o ^
     If G' |- s' : G
     (so G',V[s] |- s : G,V)
  *)
  fun invDot1 (s) = comp (comp(shift, s), invShift)


  (* Declaration Contexts *)

  (* ctxDec (G, k) = x:V
     Invariant: 
     If      |G| >= k, where |G| is size of G,
     then    G |- k : V  and  G |- V : L
  *)
  fun ctxDec (G, k) =
      let (* ctxDec' (G'', k') = x:V
	     where G |- ^(k-k') : G'', 1 <= k' <= k
           *)
	fun ctxDec' (Decl (G', Dec (x, V')), 1) = Dec (x, EClo (V', Shift (k)))
	  | ctxDec' (Decl (G', BDec (n, (l, s))), 1) = BDec (n, (l, comp (s, Shift (k))))
	  | ctxDec' (Decl (G', _), k') = ctxDec' (G', k'-1)
	 (* ctxDec' (Null, k')  should not occur by invariant *)
      in
	ctxDec' (G, k)
      end

  (* blockDec (G, v, i) = V
     
     Invariant:
     If   G (v) = l[s]
     and  Sigma (l) = SOME Gsome BLOCK Lblock
     and  G |- s : Gsome
     then G |- pi (v, i) : V
  *)

  fun blockDec (G, v as (Bidx k), i) =
    let 
      val BDec (_, (l, s)) = ctxDec (G, k)  
      (* G |- s : Gsome *)
      val (Gsome, Lblock) = conDecBlock (sgnLookup l)
      fun blockDec' (t, D :: L, 1, j) = decSub (D, t)
	| blockDec' (t, _ :: L, n, j) =
	    blockDec' (Dot (Exp (Root (Proj (v, j), Nil)), t),
			  L, n-1, j+1)
    in
      blockDec' (s, Lblock, i, 1)
    end


  (* EVar related functions *)

  (* newEVar (G, V) = newEVarCnstr (G, V, nil) *)
  fun newEVar (G, V) = EVar(ref NONE, G, V, ref nil)

  (* newAVar G = new AVar (assignable variable) *)
  (* AVars carry no type, ctx, or cnstr *)
  fun newAVar () = AVar(ref NONE)

  (* newTypeVar (G) = X, X new
     where G |- X : type
  *)
  fun newTypeVar (G) = EVar(ref NONE, G, Uni(Type), ref nil)

  (* newLVar (l, s) = (l[s]) *)
  fun newLVar (sk, (cid, t)) = LVar (ref NONE, sk, (cid, t))

  (* Definition related functions *)
  (* headOpt (U) = SOME(H) or NONE, U should be strict, normal *)
  fun headOpt (Root (H, _)) = SOME(H)
    | headOpt (Lam (_, U)) = headOpt U
    | headOpt _ = NONE

  fun ancestor' (NONE) = Anc(NONE, 0, NONE)
    | ancestor' (SOME(Const(c))) = Anc(SOME(c), 1, SOME(c))
    | ancestor' (SOME(Def(d))) =
      (case sgnLookup(d)
	 of ConDef(_, _, _, _, _, _, Anc(_, height, cOpt))
            => Anc(SOME(d), height+1, cOpt))
    | ancestor' (SOME _) = (* FgnConst possible, BVar impossible by strictness *)
      Anc(NONE, 0, NONE)
  (* ancestor(U) = ancestor info for d = U *)
  fun ancestor (U) = ancestor' (headOpt U)

  (* defAncestor(d) = ancestor of d, d must be defined *)
  fun defAncestor (d) =
      (case sgnLookup(d)
	 of ConDef(_, _, _, _, _, _, anc) => anc)

  (* Type related functions *)

  (* targetHeadOpt (V) = SOME(H) or NONE
     where H is the head of the atomic target type of V,
     NONE if V is a kind or object or have variable type.
     Does not expand type definitions.
  *)
  (* should there possibly be a FgnConst case? also targetFamOpt -kw *)
  fun targetHeadOpt (Root (H, _)) = SOME(H)
    | targetHeadOpt (Pi(_, V)) = targetHeadOpt V
    | targetHeadOpt (Redex (V, S)) = targetHeadOpt V
    | targetHeadOpt (Lam (_, V)) = targetHeadOpt V
    | targetHeadOpt (EVar (ref (SOME(V)),_,_,_)) = targetHeadOpt V
    | targetHeadOpt (EClo (V, s)) = targetHeadOpt V
    | targetHeadOpt _ = NONE
      (* Root(Bvar _, _), Root(FVar _, _), Root(FgnConst _, _),
         EVar(ref NONE,..), Uni, FgnExp _
      *)
      (* Root(Skonst _, _) can't occur *)
  (* targetHead (A) = a
     as in targetHeadOpt, except V must be a valid type
  *)
  fun targetHead (A) = valOf (targetHeadOpt A)
                      
  (* targetFamOpt (V) = SOME(cid) or NONE
     where cid is the type family of the atomic target type of V,
     NONE if V is a kind or object or have variable type.
     Does expand type definitions.
  *)
  fun targetFamOpt (Root (Const(cid), _)) = SOME(cid)
    | targetFamOpt (Pi(_, V)) = targetFamOpt V
    | targetFamOpt (Root (Def(cid), _)) = targetFamOpt (constDef cid)
    | targetFamOpt (Redex (V, S)) = targetFamOpt V
    | targetFamOpt (Lam (_, V)) = targetFamOpt V
    | targetFamOpt (EVar (ref (SOME(V)),_,_,_)) = targetFamOpt V
    | targetFamOpt (EClo (V, s)) = targetFamOpt V
    | targetFamOpt _ = NONE
      (* Root(Bvar _, _), Root(FVar _, _), Root(FgnConst _, _),
         EVar(ref NONE,..), Uni, FgnExp _
      *)
      (* Root(Skonst _, _) can't occur *)
  (* targetFam (A) = a
     as in targetFamOpt, except V must be a valid type
  *)
  fun targetFam (A) = valOf (targetFamOpt A)
                      
end;  (* functor IntSyn *)

structure IntSyn :> INTSYN =
  IntSyn (structure Global = Global);
