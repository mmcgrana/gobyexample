/*
 * Test Boogie rendering
*/

const N: int;
axiom 0 <= N;

procedure foo() {
    break;
}
// array to sort as global array, because partition & quicksort have to 
var a: [int] int;
var original: [int] int;
var perm: [int] int;

// Is array a of length N sorted?
function is_sorted(a: [int] int, l: int, r: int): bool
{
    (forall j, k: int :: l <= j && j < k && k <= r ==> a[j] <= a[k])
}

// is range a[l:r] unchanged?
function is_unchanged(a: [int] int, b: [int] int, l: int, r: int): bool {
    (forall i: int :: l <= i && i <= r ==> a[i] == b[i]) 
}

function is_permutation(a: [int] int, original: [int] int, perm: [int] int, N: int): bool
{
    (forall k: int :: 0 <= k && k < N ==> 0 <= perm[k] && perm[k] < N) &&
    (forall k, j: int :: 0 <= k && k < j && j < N ==> perm[k] != perm[j]) &&
    (forall k: int :: 0 <= k && k < N ==> a[k] == original[perm[k]])
}

function count(a: [int] int, x: int, N: int) returns (int)
{ if N == 0 then 0 else if a[N-1] == x then count(a, x, N - 1) + 1 else count(a, x, N-1) }


/*
function count(a: [int] int, x: int, N: int) returns (int)
{ if N == 0 then 0 else if a[N-1] == x then count(a, x, N - 1) + 1 else count(a, x, N-1) }

function is_permutation(a: [int] int, b: [int] int, l: int, r: int): bool {
  (forall i: int :: l <= i && i <= r ==> count(a, a[i], r+1) == count(b, a[i], r+1))
}
*/

procedure partition(l: int, r: int, N: int) returns (p: int)
    modifies a, perm;
    requires N > 0;
    requires l >= 0 && l < r && r < N;
    requires ((r+1) < N) ==> (forall k: int :: (k >= l && k <= r) ==> a[k] <= a[r+1]);
    requires ((l-1) >= 0) ==> (forall k: int :: (k >= l && k <= r) ==> a[k] > a[l-1]);

    /* a is a permutation of the original array original */
    requires is_permutation(a, original, perm, N);

    ensures (forall k: int :: (k >= l &&  k <= p ) ==> a[k] <= a[p]);
    ensures (forall k: int :: (k > p &&  k <= r ) ==> a[k] > a[p]);
    ensures p >= l && p <= r;
    ensures is_unchanged(a, old(a), 0, l-1);
    ensures is_unchanged(a, old(a), r+1, N);
    ensures ((r+1) < N) ==> (forall k: int :: (k >= l && k <= r) ==> a[k] <= a[r+1]);
    ensures ((l-1) >= 0) ==> (forall k: int :: (k >= l && k <= r) ==> a[k] > a[l-1]);

    /* a is a permutation of the original array original */
    ensures is_permutation(a, original, perm, N);
{
    var i: int;
    var sv: int;
    var pivot: int;
    var tmp: int;

    i := l;
    sv := l;
    pivot := a[r];

    while (i < r)
        invariant i <= r && i >= l;
        invariant sv <= i && sv >= l;
        invariant pivot == a[r];
        invariant (forall k: int :: (k >= l &&  k < sv) ==> a[k] <= old(a[r]));
        invariant (forall k: int :: (k >= sv &&  k < i) ==> a[k] > old(a[r]));

        /* a is a permutation of the original array original */
        invariant is_permutation(a, original, perm, N);

        invariant is_unchanged(a, old(a), 0, l-1);
        invariant is_unchanged(a, old(a), r+1, N);
        invariant ((r+1) < N) ==> (forall k: int :: (k >= l && k <= r) ==> a[k] <= a[r+1]);
        invariant ((l-1) >= 0) ==> (forall k: int :: (k >= l && k <= r) ==> a[k] > a[l-1]);
    {
        if ( a[i] <= pivot) {
            tmp := a[i]; a[i] := a[sv]; a[sv] := tmp;
            tmp := perm[i];  perm[i] := perm[sv];  perm[sv] := tmp;
            sv := sv +1;
        }
        i := i + 1;
    }

    //swap
    tmp := a[i]; a[i] := a[sv]; a[sv] := tmp;
    tmp := perm[i];  perm[i] := perm[sv];  perm[sv] := tmp;

    p := sv;
}


procedure quicksort(l: int, r: int, N: int)
    modifies a, perm;

    requires N > 0;
    requires l >= 0 && l < r && r < N;
    requires ((r+1) < N) ==> (forall k: int :: (k >= l && k <= r) ==> a[k] <= a[r+1]);
    requires ((l-1) >= 0) ==> (forall k: int :: (k >= l && k <= r) ==> a[k] > a[l-1]);

    /* a is a permutation of the original array original */
    requires is_permutation(a, original, perm, N);

    ensures ((r+1) < N) ==> (forall k: int :: (k >= l && k <= r) ==> a[k] <= a[r+1]);
    ensures ((l-1) >= 0) ==> (forall k: int :: (k >= l && k <= r) ==> a[k] > a[l-1]);

    ensures is_unchanged(a, old(a), 0, l-1);
    ensures is_unchanged(a, old(a), r+1, N);
    ensures is_sorted(a, l, r);

    /* a is a permutation of the original array original */
    ensures is_permutation(a, original, perm, N);
{
    var p: int;

    call p := partition(l, r, N);

    if ((p-1) > l) {
        call quicksort(l, p-1, N);
    }

    if ((p+1) < r) {
        call quicksort(p+1, r, N);
    }
}
