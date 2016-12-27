#
# Comments 
#

module pygments.Example

import some.Module

local function foo = |a, b| -> a + b

----
golodoc string
----
augment java.util.Collection {

  ----
  sub doc
  ----
  function plop = |this, v| {
    return this: length() + v
  }
}

function bar = |a, b| {
  let msg = "a string"
  var tmp = ""
  tmp = tmp + a: toString()
  println(tmp + b)
}

function baz = {
  foreach i in range(0, 5) {
    if i % 2 == 0 and true or false {
      print("e")
    } else {
      print("o")
    }
  }
}

function userMatch = |v| ->
  match {
    when v % 2 == 0 then "e"
    otherwise "o"
  }
}

function add = |x| -> |y| -> x + y

let aChar = 'a'

let multiline = 
"""
foo 
bar 
baz
"""

local function myObj = -> DynamicObject():
  name("foo"):
  age(25):
  define("meth", |this| -> this: name() + this: age()

----
Golo doc string
----
function nullTest = {
  let m = map[
    ["a", 1],
    ["b", 2]
  ]

  println(map: get("a") orIfNull 0)
  println(map: get("b")?: toString() orIfNull "0")

}

struct Point = { x, y }

function deco1 = |fun| {
  return |args...| {
    return "deco1 + " + fun: invokeWithArguments(args)
  }
}

@deco1
function decofoo = |a| {
  return "foo: " + a
}

@deco1
function decobar = |a| -> "bar: " + a

function deco2 = |fun| {
  return |args...| {
    return "deco2 + " + fun: invokeWithArguments(args)
  }
}

@deco2
@deco1
function decobaz = |a| -> "baz: " + a

let deco3 = ^deco1: andThen(^deco2)

@deco3
function decospam = |a| -> "spam: " + a

@another.Module.deco
function ping = -> "pong"

@deco("with", params)
function gnop = -> "gnip"
