
-- demo

define a (read cd) $ if (> a cd)
  print demo
  print "not demo"

say $ print a $ save $ b $ x $ c 8

print fun

-- test on folding

a $

b $ c

d $ e $ f

g $ h $ i j $ k $

-- test on comma

print (, a)
  a
    , b
      , c (, d)

-- test on HTML

doctype

html
  head
    title $ = Cirru
    script (:defer) $ :src build/build.js
    link (:rel stylesheet) $ :href css/page.css
    link (:rel icon)
      :href http://logo.cirru.org/cirru-32x32.png?v=3
  body
    textarea.demo.source $ :placeholder "Source Code"
    textarea.demo.target $ :placeholder "Compiled Data"
    @insert ../html/ga.html

-- test on indentation

a $ b $ c

e f
  (g)
  h

-- test on parentheses

3 4 (1) 4

((((1))))

x

-- test on quotes

a b c d

"a b c d"

"a b \" c d"

"a b" "c d"

-- test on unfolding

set
  add 1 $
  , x y
  add 5 $
  add 2

-- test on HTML attributes

div
  div
    :class a
  div
    :class a b c d

  div
    :class a (@ b) (@ c) d

  div
    :class a
      @if (@ b)
        div b
        div c
  div
    :class a
      @if (@ b) b c

-- test on helpers

@if (@call a b) (div) (span)

@each members
  div (@ name)

@each a
  div (@ b)
    @each c
      div (@ d)

-- test on HTML structure

@rich more
  #demo-more-box
    #demo-more
      :data-lang-text demo-more
    #demo-more-list
      @each room
        .demo-more-room
          span.demo-name
            @ topic
          span.demo-join
            :data-lang-text demo-join
            :data-id (@ id)

-- text on bool

print #true
print #false
print #yes
print #no
print #t
print #f

-- test on Cirru js

set a 1
set a (= "This is a string")
set b #t

-- this is comment

number 1.4
string x
regex ^\s$
regex "^\\s-\"$"
sentence this is a string

array 1 2 3 (= nothing) #t (= #t)

set c (array 1 (= nothing))

set d $ object (a (= google))
  b (= reader)
  c 1
  d $ array 1 2 (= string)

1 c
-1 c

:b d
.log console a 2
.log console

set demo $ object
  call $ \ x (.log console x) (. this call)
. demo (.call 1) (.call 4)

=.x d 3

set d null

new Array 1 2 3

set x (:length c)
set str (= str)
set c (.toUpperCase str)

\ x (+ x 1)
\ (x y) (+ x y)
\ x (set aa 1) (+ aa x)

set f (\ x (+ x 1))

+ a 1 2
+= a 1

> 1 2 3

if (> 2 1) (+ a 1)
else 2

if (> a 2)
  .log console (= "large")
elseif (> a 1)
  .log console (= "still good")
else
  .log console (= "so so")

set a $ if (> 2 1) #t #f

switch a
  1 (.log console 1)
  2 (.log console 2)
  else (.log console (= "something else"))

set a $ array 2 +3 -4
for (a x i) (.log console x i)

set a 0
while (< a 10) (+= a 1) (.log console a)

-- WebAssembly variable names

-- ":(c) 2015 Andreas Rossberg"

module
  export :even $even
  export "odd" $odd

  func $even (param $n i32) (result i32)
    if (i32.eq (get_local $n) (i32.const 0))
      i32.const 1
      call $odd (i32.sub (get_local $n) (i32.const 1))

  func $odd (param $n i32) (result i32)
    store_global $scratch (get_local $n)
    if (i32.eq (get_local $n) (i32.const 0)
      i32.const 0
      call $even (i32.sub (get_local $n) (i32.const 1))

  global $scratch i32

assert_eq (invoke :even (i32.const 13)) (i32.const 0)
assert_eq (invoke :even (i32.const 20)) (i32.const 1)
assert_eq (invoke :odd (i32.const 13)) (i32.const 1)
assert_eq (invoke :odd (i32.const 20)) (i32.const 0)
