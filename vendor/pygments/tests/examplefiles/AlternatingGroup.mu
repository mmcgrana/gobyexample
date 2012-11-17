/*++ $Id: AlternatingGroup.mu,v 1.4 2003/09/08 15:00:47 nthiery Exp $

Dom::AlternatingGroup(n) -- the Alternating Group of {1..n}

n	   - integer >= 1

Elements are represented as in Dom::PermutationGroup(n)

Author:	     Nicolas M. Thiéry <nthiery@users.sourceforge.net>
License:     LGPL
Created:     August 8th, 1999
Last update: $Date: 2003/09/08 15:00:47 $
++*/

domain Dom::AlternatingGroup(n: Type::PosInt)
    inherits Dom::PermutationGroup(n,toBeDefined);
    category Cat::PermutationGroup;
    axiom Ax::canonicalRep;

/*--
    size

    Size of the group.
--*/

    size := fact(n)/2;

/*--
    generators

    A list of generators of the group

    The first 3-cycle (1,2,3), and a maximal even cycle (1,...,n) or
    (2,...,n) depending on the parity of n

--*/

    generators :=
    if	 n<=2	     then generators:=[dom([[1]])];
    elif n=3	     then generators:=[dom([[1,2,3]])];
    elif n mod 2=0   then generators:=[dom([[1,2,3]]), dom([[$2..n]])];
    else		  generators:=[dom([[1,2,3]]), dom([[$1..n]])];
    end_if;
    
/*--
    allElements

    List of all the elements of the group
--*/

    allElements :=
    proc()
	option remember;
	local p;
    begin
	[new(dom,p) $ p in select(combinat::permutations(n),
				  p->bool(combinat::permutations::sign(p)=1))];
    end_proc;

/*--
    cycleTypes:

    Count the elements of the group by cycle type.
    (Cf Cat::PermutationGroupModule).

    Same algorithm as for Dom::SymmetricGroup, but only even permutations
    are considered. This is done by disregarding partitions p such
    that n-length(p) is odd.
--*/

    cycleTypes :=
    proc()
	option remember;
	local t, p, gen;
    begin
	userinfo(3, "cycleTypes: starting computation");
	t:=table();

	gen := combinat::partitions::generator(n);
	while (p:=gen()) <> FAIL do
	    userinfo(5, "working on partition", p);
	    if(n-nops(p) mod 2=0) then
		// Compute the size of the conjugacy class of Sn indexed by p
		// and the cycle type of a permutation in this conjugacy class
                t[combinat::partitions::toExp(p,n)]
                  := combinat::partitions::conjugacyClassSize(p);
	    end_if;
        end_while;
	t;
    end_proc;

begin
    if testargs() then
	if args(0) <> 1 then error("wrong no of args"); end_if;
	if not testtype(n,DOM_INT) then
	    error("argument must be integer")
	end_if;
	if n < 1 then
	    error("argument must be positive")
	end_if;
    end_if;
end_domain:
