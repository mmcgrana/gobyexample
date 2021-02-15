/***********************************************************************
 * Chapel implementation of "99 bottles of beer"
 *
 * by Brad Chamberlain and Steve Deitz
 * 07/13/2006 in Knoxville airport while waiting for flight home from
 *            HPLS workshop
 * compiles and runs with chpl compiler version 1.12.0
 * for more information, contact: chapel_info@cray.com
 * 
 *
 * Notes: 
 * o as in all good parallel computations, boundary conditions
 *   constitute the vast bulk of complexity in this code (invite Brad to
 *   tell you about his zany boundary condition simplification scheme)
 * o uses type inference for variables, arguments
 * o relies on integer->string coercions
 * o uses named argument passing (for documentation purposes only)
 ***********************************************************************/
 
// allow executable command-line specification of number of bottles 
// (e.g., ./a.out -snumBottles=999999)
config const numBottles = 99;
const numVerses = numBottles+1;
 
// a domain to describe the space of lyrics
var LyricsSpace: domain(1) = {1..numVerses};
 
// array of lyrics
var Lyrics: [LyricsSpace] string;
 
// parallel computation of lyrics array
[verse in LyricsSpace] Lyrics(verse) = computeLyric(verse);
 
// as in any good parallel language, I/O to stdout is serialized.
// (Note that I/O to a file could be parallelized using a parallel
// prefix computation on the verse strings' lengths with file seeking)
writeln(Lyrics);
 
 
// HELPER FUNCTIONS:
 
proc computeLyric(verseNum) {
  var bottleNum = numBottles - (verseNum - 1);
  var nextBottle = (bottleNum + numVerses - 1)%numVerses;
  return "\n" // disguise space used to separate elements in array I/O
    + describeBottles(bottleNum, startOfVerse=true) + " on the wall, "
    + describeBottles(bottleNum) + ".\n"
    + computeAction(bottleNum)
    + describeBottles(nextBottle) + " on the wall.\n";
}
 
 
proc describeBottles(bottleNum, startOfVerse:bool = false) {
  // NOTE: bool should not be necessary here (^^^^); working around bug
  var bottleDescription = if (bottleNum) then bottleNum:string 
    else (if startOfVerse then "N" 
            else "n") 
           + "o more";
  return bottleDescription 
    + " bottle" + (if (bottleNum == 1) then "" else "s") 
    + " of beer";
}
 
 
proc computeAction(bottleNum) {
  return if (bottleNum == 0) then "Go to the store and buy some more, "
    else "Take one down and pass it around, ";
}


// Modules...
module M1 {
  var x = 10;

  var y = 13.0;
}

module M2 {
  use M1 except y;
  use M1 only y;
  proc main() {
    writeln("M2 -> M1 -> x " + x);
  }
}


// Classes, records, unions...
const PI: real = 3.14159;

record Point {
  var x, y: real;
}
var p: Point;
writeln("Distance from origin: " + sqrt(p.x ** 2 + p.y ** 2)); 
p = new Point(1.0, 2.0);
writeln("Distance from origin: " + sqrt(p.x ** 2 + p.y ** 2)); 

class Circle {
  var p: Point;
  var r: real;
}
var c = new Circle(r=2.0);
proc Circle.area()
  return PI * r ** 2;
writeln("Area of circle: " + c.area());

class Oval: Circle {
  var r2: real;
}
proc Oval.area()
  return PI * r * r2;

delete c;
c = nil;
c = new Oval(r=1.0, r2=2.0);
writeln("Area of oval: " + c.area());

// This is a valid decimal integer:
var x = 0000000000012;

union U {
  var i: int;
  var r: real;
}

// chapel ranges are awesome.
var r1 = 1..10,            // 1 2 3 4 5 6 7 8 9 10
  r2 = 10..1,              // no values in this range
  r3 = 1..10 by -1,        // 10 9 8 7 6 5 4 3 2 1
  r4 = 1..10 by 2,         // 1 3 5 7 9
  r5 = 1..10 by 2 align 0, // 2 4 6 8 10
  r6 = 1..10 by 2 align 2, // 2 4 6 8 10
  r7 = 1..10 # 3,          // 1 2 3
  r8 = 1..10 # -2,         // 9 10
  r9 = 1..100 # 10 by 2,   // 1 3 5 7 9
  ra = 1..100 by 2 # 10,   // 1 3 5 7 9 11 13 15 17 19
  rb = 1.. # 100 by 10;    // 1 11 21 31 41 51 61 71 81 91

// create a variable with default initialization
var myVarWithoutInit: real = noinit;
myVarWithoutInit = 1.0;

// Chapel has <~> operator for read and write I/O operations.
class IntPair {
  var x: int;
  var y: int;
  proc readWriteThis(f) {
    f <~> x <~> new ioLiteral(",") <~> y <~> new ioNewline();
  }
}
var ip = new IntPair(17,2);
write(ip);

var targetDom = {1..10},
  target: [targetDom] int;
coforall i in targetDom with (ref target) {
  target[i] = i ** 3;
}

var wideOpen = 0o777,
  mememe = 0o600,
  clique_y = 0O660,
  zeroOct = 0o0,
  minPosOct = 0O1;

private module M3 {
  private proc foo() {

  }

  private iter bar() {
    for i in 1..10 {
      yield i;
    }
  }

  private var x: int;

}
