preface.... everything what is before header is not evaluated
so this should not be colorized:
1 + 2

REBOL [] ;<- this is minimal header, everything behind it must be colorized

;## String tests ##
print "Hello ^"World" ;<- with escaped char
multiline-string: {
    bla bla "bla" {bla}
}
char-a: #"a"
escaped-a: #"^(61)"
new-line: #"^/"

;## Binaries ##
print decompress 64#{eJzLSM3JyQcABiwCFQUAAAA=}
;2#{0000 00000} ;<- this one is invalid!
2#{}
#{FF00}

;##Date + time ##
1-Feb-2009
1-Feb-2009/2:24:46+1:0
1:0 1:1:1 -0:1.1

;## Tuple ##
red: 255.0.0
red-with-alpha: 255.0.0.100

;## url!, file! and email! ##
aaa@bbb.cz
http://
dns://
tcp://127.0.0.1
%/c/rebol/
%"c:\Program Files\"
%/c/Program%20Files/
to-rebol-file "c:\Program Files\"
suffix? %bla.swf

;## Money ##
$1
-$1.2
USA$100

;## Tag! ##
<a>
<a href="a()">

;## Pair! ##
10x200

;## Issue! ##
type? #ff0000 ;== issue!

;## some numbers ##
to integer! (1 + (x / 4.5) * 1E-4)

;## some spec comments
1 + 1
comment "aa"								
2 + 2
comment {aa}
3 + 3
comment {a^{}
4 + 4
comment {{}}
5 + 5
comment {
	foo: 6
}
6 + 6
comment [foo: 6]
7 + 7
comment [foo: "[" ]
8 + 8
comment [foo: {^{} ]
9 + 9
comment [foo: {boo} ]
10 + 10
comment 5-May-2014/11:17:34+2:00
5-May-2014/11:17:34+2:00 11 + 11	

;## other tests ##
---: 1
x/(1 + n)/y
b/:1

;## and...
REBOL [
    purpose: {
        reads css file and creates html from it
        so one can see how the styles looks like
    } 
]
style: %default
out: rejoin [{
<html>
<head>
  <title>Pygments style: } style {.css</title>
  <link rel="stylesheet" href="} style {.css">
</head>
<body>
<div class="syntax"><pre>
}]
css: read/lines join style %.css
foreach line css [
    parse line [".syntax ." copy c to " " thru "/*" copy t to "*/" to end (
        append out rejoin ["<span class=" c ">" t "</span>^/"])
    ]
]
write join style %.html join out "</pre></div></body></html>"
halt
