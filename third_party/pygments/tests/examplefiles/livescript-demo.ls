a = -> [1 to 50]
const b = --> [2 til 5]
var c = ~~> 10_000_000km * 500ms - 16~ff / 32~lol
e = (a) -> (b) ~> (c) --> (d, e) ~~> <[list of words]>
dashes-identifiers = ->
  a - a b -- c 1-1 1- -1 a- a a -a
underscores_i$d = ->
  /regexp1/
  //regexp2//g
  'strings' and "strings" and \strings and \#$-"\'strings

another-word-list = <[ more words ]>

[2 til 10]
  |> map (* 2)
  |> filter (> 5)
  |> fold (+)

obj =
  prop1: 1
  prop2: 2

class Class extends Anc-est-or
  (args) ->
    <- # Comment
    <~ /* Comment */
    void undefined yes no on off
    a.void b.undefined c.off d.if f.no g.not
    avoid bundefined coff dif fno gnot
    "inter #{2 + 2} #variable"
    '''HELLO 'world' '''

copy = (from, to, callback) -->
  error, data <- read file
  return callback error if error?
  error <~ write file, data
  return callback error if error?
  callback()

take(n, [x, ...xs]:list) =
  | n <= 0     => []
  | empty list => []
  | otherwise  => [x] +++ take n - 1, xs
