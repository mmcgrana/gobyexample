// Koka language test module

// This module implements the GarsiaWachs algorithm.
// It is an adaptation of the algorithm in ML as described by JeanChristophe Filli�tre:
// in ''A functional implementation of the GarsiaWachs algorithm. (functional pearl). ML workshop 2008, pages 91--96''.
// See: http://www.lri.fr/~filliatr/publis/gwWml08.pdf
//
// The algorithm is interesting since it uses mutable references shared between a list and tree but the
// side effects are not observable from outside. Koka automatically infers that the final algorithm is pure.
// Note: due to a current limitation in the divergence analysis, koka cannot yet infer that mutually recursive
// definitions in "insert" and "extract" are terminating and the final algorithm still has a divergence effect.
// However, koka does infer that no other effect (i.e. an exception due to a partial match) can occur.
module garcsiaWachs

import test = qualified std/flags

# pre processor test

public function main() {
  wlist = Cons1(('a',3), [('b',2),('c',1),('d',4),('e',5)])
  tree  = wlist.garsiaWachs()
  tree.show.println()
}

//----------------------------------------------------
// Trees
//----------------------------------------------------
public type tree<a> {
  con Leaf(value :a)
  con Node(left :tree<a>, right :tree<a>)
}

function show( t : tree<char> ) : string {
  match(t) {
    Leaf(c) -> core/show(c)  
    Node(l,r) -> "Node(" + show(l) + "," + show(r) + ")"
  }
}


//----------------------------------------------------
// Non empty lists
//----------------------------------------------------
public type list1<a> {
  Cons1( head : a, tail : list<a> )
}

function map( xs, f ) {
  val Cons1(y,ys) = xs
  return Cons1(f(y), core/map(ys,f))
}

function zip( xs :list1<a>, ys :list1<b> ) : list1<(a,b)> {
  Cons1( (xs.head, ys.head), zip(xs.tail, ys.tail))
}


//----------------------------------------------------
// Phase 1
//----------------------------------------------------

function insert( after : list<(tree<a>,int)>, t : (tree<a>,int), before : list<(tree<a>,int)> ) : div tree<a>
{
  match(before) {
    Nil -> extract( [], Cons1(t,after) )
    Cons(x,xs) -> {
      if (x.snd < t.snd) then return insert( Cons(x,after), t, xs )
      match(xs) {
        Nil        -> extract( [], Cons1(x,Cons(t,after)) )
        Cons(y,ys) -> extract( ys, Cons1(y,Cons(x,Cons(t,after))) )
      }
    }
  }
}

function extract( before : list<(tree<a>,int)>, after : list1<(tree<a>,int)> ) : div tree<a>
{
  val Cons1((t1,w1) as x, xs ) = after
  match(xs) {
    Nil -> t1
    Cons((t2,w2) as y, ys) -> match(ys) {
      Nil -> insert( [], (Node(t1,t2), w1+w2), before )
      Cons((_,w3),_zs) ->
        if (w1 <= w3)
         then insert(ys, (Node(t1,t2), w1+w2), before)
         else extract(Cons(x,before), Cons1(y,ys))
    }
  }
}

function balance( xs : list1<(tree<a>,int)> ) : div tree<a> {
  extract( [], xs )
}

//----------------------------------------------------
// Phase 2
//----------------------------------------------------

function mark( depth :int, t :tree<(a,ref<h,int>)> ) : <write<h>> () {
  match(t) {
    Leaf((_,d)) -> d := depth
    Node(l,r)   -> { mark(depth+1,l); mark(depth+1,r) }
  }
}

function build( depth :int, xs :list1<(a,ref<h,int>)> ) : <read<h>,div> (tree<a>,list<(a,ref<h,int>)>)
{
  if (!(xs.head.snd) == depth) return (Leaf(xs.head.fst), xs.tail)

  l = build(depth+1, xs)
  match(l.snd) {
    Nil -> (l.fst, Nil)
    Cons(y,ys) -> {
      r = build(depth+1, Cons1(y,ys))
      (Node(l.fst,r.fst), r.snd)
    }
  }
}

//----------------------------------------------------
// Main
//----------------------------------------------------

public function garsiaWachs( xs : list1<(a,int)> ) : div tree<a>
{
  refs   = xs.map(fst).map( fun(x) { (x, ref(0)) } )
  wleafs = zip( refs.map(Leaf), xs.map(snd) )

  tree = balance(wleafs)
  mark(0,tree)
  build(0,refs).fst
}

