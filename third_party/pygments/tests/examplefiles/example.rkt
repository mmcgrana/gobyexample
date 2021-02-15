#lang racket

(require (only-in srfi/13 string-contains))

; Single-line comment style.

;; Single-line comment style.

#| Multi-line comment style ... on one line |#

#|
Multi-line comment style ...
#|### #| nested |#||| |#
... on multiple lines
|#

#;(s-expression comment (one line))

#;
(s-expression comment
              (multiple lines))

#! shebang comment

#!/shebang comment

#! shebang \
comment

#!/shebang \
comment

;; Uncommented numbers after single-line comments
;; NEL133
;; LS 8232
;; PS 8233

#reader racket
(define(a-function x #:keyword [y 0])
  (define foo0 'symbol) ; ()
  [define foo1 'symbol] ; []
  {define foo2 'symbol} ; {}
  (define 100-Continue 'symbol)
  (and (append (car'(1 2 3))))
  (regexp-match? #rx"foobar" "foobar")
  (regexp-match? #px"\"foo\\(bar\\)?\"" "foobar")
  (regexp-match? #rx#"foobar" "foobar")
  (regexp-match? #px#"foobar" "foobar")
  (define #csa 1)
  #Ci (let ([#%A|||b #true C
\|ｄ "foo"])
        (displayln #cS #%\ab\ #true\ C\
\\ｄ||))
  (for/list ([x (in-list (list 1 2 (list 3 4)))])
    (cond
      [(pair? x) (car x)]
      [else x])))

;; Literals
(values
 ;; #b
 #b1
 #b+1
 #b-1
 #b.1
 #b1.
 #b0.1
 #b+0.1
 #b-0.1
 #b1/10
 #b+1/10
 #b-1/10
 #b1e11
 #b+1e11
 #b-1e11
 #b.1e11
 #b1.e11
 #b0.1e11
 #b+0.1e11
 #b-0.1e11
 #b1/10e11
 #b+1/10e11
 #b-1/10e11
 #b+i
 #b1+i
 #b+1+i
 #b-1+i
 #b.1+i
 #b1.+i
 #b0.1+i
 #b+0.1+i
 #b-0.1+i
 #b1/10+i
 #b+1/10+i
 #b-1/10+i
 #b1e11+i
 #b+1e11+i
 #b-1e11+i
 #b1.e11+i
 #b.1e11+i
 #b0.1e11+i
 #b+0.1e11+i
 #b-0.1e11+i
 #b1/10e11+i
 #b+1/10e11+i
 #b-1/10e11+i
 #b+1i
 #b1+1i
 #b+1+1i
 #b-1+1i
 #b1.+1i
 #b.1+1i
 #b0.1+1i
 #b+0.1+1i
 #b-0.1+1i
 #b1/10+1i
 #b+1/10+1i
 #b-1/10+1i
 #b1e11+1i
 #b+1e11+1i
 #b-1e11+1i
 #b.1e11+1i
 #b0.1e11+1i
 #b+0.1e11+1i
 #b-0.1e11+1i
 #b1/10e11+1i
 #b+1/10e11+1i
 #b-1/10e11+1i
 #b+1/10e11i
 #b1+1/10e11i
 #b+1+1/10e11i
 #b-1+1/10e11i
 #b.1+1/10e11i
 #b0.1+1/10e11i
 #b+0.1+1/10e11i
 #b-0.1+1/10e11i
 #b1/10+1/10e11i
 #b+1/10+1/10e11i
 #b-1/10+1/10e11i
 #b1e11+1/10e11i
 #b+1e11+1/10e11i
 #b-1e11+1/10e11i
 #b.1e11+1/10e11i
 #b0.1e11+1/10e11i
 #b+0.1e11+1/10e11i
 #b-0.1e11+1/10e11i
 #b1/10e11+1/10e11i
 #b+1/10e11+1/10e11i
 #b-1/10e11+1/10e11i
 ;; #d
 #d1
 #d+1
 #d-1
 #d.1
 #d1.
 #d1.2
 #d+1.2
 #d-1.2
 #d1/2
 #d+1/2
 #d-1/2
 #d1e3
 #d+1e3
 #d-1e3
 #d.1e3
 #d1.e3
 #d1.2e3
 #d+1.2e3
 #d-1.2e3
 #d1/2e3
 #d+1/2e3
 #d-1/2e3
 #d+i
 #d1+i
 #d+1+i
 #d-1+i
 #d.1+i
 #d1.+i
 #d1.2+i
 #d+1.2+i
 #d-1.2+i
 #d1/2+i
 #d+1/2+i
 #d-1/2+i
 #d1e3+i
 #d+1e3+i
 #d-1e3+i
 #d1.e3+i
 #d.1e3+i
 #d1.2e3+i
 #d+1.2e3+i
 #d-1.2e3+i
 #d1/2e3+i
 #d+1/2e3+i
 #d-1/2e3+i
 #d+1i
 #d1+1i
 #d+1+1i
 #d-1+1i
 #d1.+1i
 #d.1+1i
 #d1.2+1i
 #d+1.2+1i
 #d-1.2+1i
 #d1/2+1i
 #d+1/2+1i
 #d-1/2+1i
 #d1e3+1i
 #d+1e3+1i
 #d-1e3+1i
 #d.1e3+1i
 #d1.2e3+1i
 #d+1.2e3+1i
 #d-1.2e3+1i
 #d1/2e3+1i
 #d+1/2e3+1i
 #d-1/2e3+1i
 #d+1/2e3i
 #d1+1/2e3i
 #d+1+1/2e3i
 #d-1+1/2e3i
 #d.1+1/2e3i
 #d1.2+1/2e3i
 #d+1.2+1/2e3i
 #d-1.2+1/2e3i
 #d1/2+1/2e3i
 #d+1/2+1/2e3i
 #d-1/2+1/2e3i
 #d1e3+1/2e3i
 #d+1e3+1/2e3i
 #d-1e3+1/2e3i
 #d.1e3+1/2e3i
 #d1.2e3+1/2e3i
 #d+1.2e3+1/2e3i
 #d-1.2e3+1/2e3i
 #d1/2e3+1/2e3i
 #d+1/2e3+1/2e3i
 #d-1/2e3+1/2e3i
 ;; Extflonums
 +nan.t
 1t3
 +1t3
 -1t3
 .1t3
 1.t3
 1.2t3
 +1.2t3
 -1.2t3
 1/2t3
 +1/2t3
 -1/2t3
 1#t0
 1.#t0
 .2#t0
 1.2#t0
 1#/2t0
 1/2#t0
 1#/2#t0
 1#t3
 1.#t3
 .2#t3
 1.2#t3
 1#/2t3
 1/2#t3
 1#/2#t3
 ;; No # reader prefix -- same as #d
 -1.23
 1.123
 1e3
 1e-22
 1/2
 -1/2
 1
 -1
 ;; #e
 #e-1.23
 #e1.123
 #e1e3
 #e1e-22
 #e1
 #e-1
 #e1/2
 #e-1/2
 ;; #d#e
 #d#e-1.23
 #d#e1.123
 #d#e1e3
 #d#e1e-22
 #d#e1
 #d#e-1
 #d#e1/2
 #d#e-1/2
 ;; #e#d
 #e#d-1.23
 #e#d1.123
 #e#d1e3
 #e#d1e-22
 #e#d1
 #e#d-1
 #e#d1/2
 #e#d-1/2
 ;; #i always float
 #i-1.23
 #i1.123
 #i1e3
 #i1e-22
 #i1/2
 #i-1/2
 #i1
 #i-1
 ;; Implicitly inexact numbers
 +nan.0
 1#
 1.#
 .2#
 1.2#
 1#/2
 1/2#
 1#/2#
 1#e3
 1.#e3
 .2#e3
 1.2#e3
 1#/2e3
 1/2#e3
 1#/2#e3
 +nan.0+i
 1#+i
 1.#+i
 .2#+i
 1.2#+i
 1#/2+i
 1/2#+i
 1#/2#+i
 1#e3+i
 1.#e3+i
 .2#e3+i
 1.2#e3+i
 1#/2e3+i
 1/2#e3+i
 1#/2#e3+i
 +nan.0i
 +1#i
 +1.#i
 +.2#i
 +1.2#i
 +1#/2i
 +1/2#i
 +1#/2#i
 +1#e3i
 +1.#e3i
 +.2#e3i
 +1.2#e3i
 +1#/2e3i
 +1/2#e3i
 +1#/2#e3i
 0+nan.0i
 0+1#i
 0+1.#i
 0+.2#i
 0+1.2#i
 0+1#/2i
 0+1/2#i
 0+1#/2#i
 0+1#e3i
 0+1.#e3i
 0+.2#e3i
 0+1.2#e3i
 0+1#/2e3i
 0+1/2#e3i
 0+1#/2#e3i
 1#/2#e3+nan.0i
 1#/2#e3+1#i
 1#/2#e3+1.#i
 1#/2#e3+.2#i
 1#/2#e3+1.2#i
 1#/2#e3+1#/2i
 1#/2#e3+1/2#i
 1#/2#e3+1#/2#i
 1#/2#e3+1#e3i
 1#/2#e3+1.#e3i
 1#/2#e3+.2#e3i
 1#/2#e3+1.2#e3i
 1#/2#e3+1#/2e3i
 1#/2#e3+1/2#e3i
 1#/2#e3+1#/2#e3i
 +nan.0@1
 1#@1
 1.#@1
 .2#@1
 1.2#@1
 1#/2@1
 1/2#@1
 1#/2#@1
 1#e3@1
 1.#e3@1
 .2#e3@1
 1.2#e3@1
 1#/2e3@1
 1/2#e3@1
 1#/2#e3@1
 1@+nan.0
 1@1#
 1@1.#
 1@.2#
 1@1.2#
 1@1#/2
 1@1/2#
 1@1#/2#
 1@1#e3
 1@1.#e3
 1@.2#e3
 1@1.2#e3
 1@1#/2e3
 1@1/2#e3
 1@1#/2#e3
 1#/2#e3@1#
 1#/2#e3@1.#
 1#/2#e3@.2#
 1#/2#e3@1.2#
 1#/2#e3@1#/2
 1#/2#e3@1/2#
 1#/2#e3@1#/2#
 1#/2#e3@1#e3
 1#/2#e3@1.#e3
 1#/2#e3@.2#e3
 1#/2#e3@1.2#e3
 1#/2#e3@1#/2e3
 1#/2#e3@1/2#e3
 1#/2#e3@1#/2#e3
 ;; #o
 #o777.777
 #o-777.777
 #o777e777
 #o777e-777
 #o3/7
 #o-3/7
 #o777
 #o-777
 #e#o777.777
 #e#o-777.777
 #e#o777e777
 #e#o777e-777
 #e#o3/7
 #e#o-3/7
 #e#o777
 #e#o-777
 #i#o777.777
 #i#o-777.777
 #i#o777e777
 #i#o777e-777
 #i#o3/7
 #i#o-3/7
 #i#o777
 #i#o-777
 ;; #x
 #x-f.f
 #xf.f
 #xfsf
 #xfs-f
 #x7/f
 #x-7/f
 #x-f
 #xf
 #e#x-f.f
 #e#xf.f
 #e#xfsf
 #e#xfs-f
 #e#x7/f
 #e#x-7/f
 #e#x-f
 #e#xf
 #i#x-f.f
 #i#xf.f
 #i#xfsf
 #i#xfs-f
 #i#x7/f
 #i#x-7/f
 #i#x-f
 #i#xf
 ;; Not numbers
 '-1.23x
 '1.123x
 '1e3x
 '1e-22x
 '1/2x
 '-1/2x
 '1x
 '-1x
 '/
 '1/
 '/2
 '1//2
 '1e3.
 '1e
 'e3
 '.i
 '1.2.3
 '1..2
 '.1.
 '@
 '1@
 '@2
 '1@@2
 '1@2@3
 '1@2i
 '1+-2i
 '1i+2
 '1i+2i
 '1+2i+3i
 '-
 '--1
 '+
 '++1
 '1/2.3
 '1#2
 '1#.2
 '1.#2
 '.#2
 '+nan.t+nan.ti
 '+nan.t@nan.t
 ;; Booleans
 #t
 #T
 #true
 #f
 #F
 #false
 ;; Characters, strings, and byte strings
 #\
 #\Null9
 #\n9
 #\99
 #\0009
 #\u3BB
 #\u03BB9
 #\U3BB
 #\U000003BB9
 #\λ9
 "string\
 \a.\b.\t.\n.\v.\f.\r.\e.\".\'.\\.\1.\123.\1234.\x9.\x30.\x303"
 "\u9.\u1234.\u12345.\U9.\U00100000.\U001000000"
 #"byte-string\7\xff\t"
 #<<HERE STRING
lorem ipsum
dolor sit amet
consectetur HERE STRING
HERE STRING adipisicing elit
HERE STRING
 #|
HERE STRING
|#
 ;; Other literals
 #(vector)
 #20()
 #s[prefab-structure 1 2 3]
 #&{box}
 #hash(("a" . 5))
 #hasheq((a . 5) (b . 7))
 #hasheqv((a . 5) (b . 7))
 #'(define x 1)
 #`(define x #,pi)
 ;; quote, quasiquote, and unquote
 'pi
 ' pi
 ''pi
 '`pi
 '`,pi
 ',pi
 `pi
 ` pi
 `'pi
 ``pi
 `,pi
 ` , pi
 `,'pi
 `,`pi
 `,`,pi
 '(+)
 ' (+)
 ''(+)
 '`(+)
 ',(+)
 `(+)
 ` (+)
 `'(+)
 ``(+)
 `,(+)
 ` , (+)
 `,'(+)
 `,`(+)
 `,`,(+)
 #readerracket/base'pi.f
 '#readerracket/base pi.f
 #readerracket/base`pi.f
 `#readerracket/base pi.f
 #readerracket/base`,pi.f
 `#readerracket/base,pi.f
 `,#readerracket/base pi.f
 #readerracket/base'`,pi.f
 '#readerracket/base`,pi.f
 '`#readerracket/base,pi.f
 '`,#readerracket/base pi.f
 #readerracket/base'(*)
 '#readerracket/base(*)
 #readerracket/base`(*)
 `#readerracket/base(*)
 #readerracket/base`,(*)
 `#readerracket/base,(*)
 `,#readerracket/base(*)
 #readerracket/base'`,(*)
 '#readerracket/base`,(*)
 '`#readerracket/base,(*)
 '`,#readerracket/base(*)
 (quote pi)
 (quote (quote pi))
 (quote (quasiquote pi))
 (quote (quasiquote (unquote pi)))
 (quote (unquote pi))
 (quasiquote pi)
 (quasiquote (quote pi))
 (quasiquote (quasiquote pi))
 (quasiquote (unquote pi))
 (quasiquote (unquote (quote pi)))
 (quasiquote (unquote (quasiquote pi)))
 (quasiquote (unquote (quasiquote (unquote pi))))
 (quote (+))
 (quote (quote (+)))
 (quote (quasiquote (+)))
 (quote (unquote (+)))
 (quasiquote (+))
 (quasiquote (quote (+)))
 (quasiquote (quasiquote (+)))
 (quasiquote (unquote (+)))
 (quasiquote (unquote (quote (+))))
 (quasiquote (unquote (quasiquote (+))))
 (quasiquote (unquote (quasiquote (unquote (+)))))
 #reader racket/base (quote pi.f)
 (quote #reader racket/base pi.f)
 #reader racket/base (quasiquote pi.f)
 (quasiquote #reader racket/base pi.f)
 #reader racket/base (quasiquote (unquote pi.f))
 (quasiquote #reader racket/base (unquote pi.f))
 (quasiquote (unquote #reader racket/base pi.f))
 #reader racket/base (quote (quasiquote (unquote pi.f)))
 (quote #reader racket/base (quasiquote (unquote pi.f)))
 (quote (quasiquote #reader racket/base (unquote pi.f)))
 (quote (quasiquote (unquote #reader racket/base pi.f)))
 #reader racket/base (quote (*))
 (quote #reader racket/base (*))
 #reader racket/base (quasiquote (*))
 (quasiquote #reader racket/base (*))
 #reader racket/base (quasiquote (unquote (*)))
 (quasiquote #reader racket/base (unquote (*)))
 (quasiquote (unquote #reader racket/base (*)))
 #reader racket/base (quote (quasiquote (unquote (*))))
 (quote #reader racket/base (quasiquote (unquote (*))))
 (quote (quasiquote #reader racket/base (unquote (*))))
 (quote (quasiquote (unquote #reader racket/base (*))))
 ;; Make sure non-identifiers work with quotes
 ' "" pi
 ' #t pi
 ' #() pi
 ' #s(s) pi
 ' #\u3BB pi
 ' #\U000003BB pi
 ' #\space pi
 ' #\. pi
 ' #"" pi
 ' #:kw pi
 ' #&b pi
 ' #'(define x 1) pi
 ' #`(define x #,pi) pi
 ' #I0 pi
 ' #E0 pi
 ' #X0 pi
 ' #O0 pi
 ' #D0 pi
 ' #B0 pi
 ' #<<EOF
EOF
 pi
 ' #rx"" pi
 ' #rx#"" pi
 ' #px"" pi
 ' #px#"" pi
 ' #hash() pi
 ' #hasheq[] pi
 ' #hasheqv{} pi
 ' #1(v) pi
 )

;; Use the following to generate lists of built-ins and keywords.
;; Run
;;   (displayln (wrap-lines KEYWORDS))
;;   (displayln (wrap-lines BUILTINS))
;; and copy the results into RacketLexer._keywords and RacketLexer._builtins.

;; (-> (listof string?) string?)
;; Appends all the strings together, quoting them as appropriate for Python,
;; with commas and spaces between them, wrapping at 80 characters, with an
;; indentation of 8 spaces.
(define (wrap-lines lst)
  (define INDENTATION '"        ")
  (define WIDTH '80)
  (define (wrap-lines* lst done-lines current-line)
    (if (null? lst)
        (string-append (foldr string-append "" done-lines) current-line)
        (let* ([str (first lst)]
               [wrapped-str (if (regexp-match-exact? '#px"[[:ascii:]]+" str)
                                (string-append "'" str "',")
                                (string-append "u'" str "',"))]
               [new-line (string-append current-line " " wrapped-str)])
          (if ((string-length new-line) . >= . WIDTH)
              (wrap-lines* (rest lst)
                           (append done-lines
                                   `(,(string-append current-line "\n")))
                           (string-append INDENTATION wrapped-str))
              (wrap-lines* (rest lst)
                           done-lines
                           new-line)))))
  (wrap-lines* lst '() INDENTATION))

;; (-> string? boolean?)
;; Returns #t if str represents a syntax identifier in the current namespace,
;; otherwise #f.
(define (syntax-identifier? str)
    (with-handlers ([exn? exn?])
      (not (eval (call-with-input-string str read)))))

(define RACKET-NAMESPACE
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require 'racket)
    (current-namespace)))

(define BOUND-IDENTIFIERS
  (parameterize ([current-namespace RACKET-NAMESPACE])
    (sort (map symbol->string (namespace-mapped-symbols))
          string<=?)))

(define-values (KEYWORDS BUILTINS)
  (parameterize ([current-namespace RACKET-NAMESPACE])
    (partition syntax-identifier? BOUND-IDENTIFIERS)))
