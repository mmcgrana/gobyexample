.. -*- mode: rst -*-

===================================
Using Pygments in various scenarios
===================================

Markdown
--------

Since Pygments 0.9, the distribution ships Markdown_ preprocessor sample code
that uses Pygments to render source code in
:file:`external/markdown-processor.py`.  You can copy and adapt it to your
liking.

.. _Markdown: http://www.freewisdom.org/projects/python-markdown/

TextMate
--------

Antonio Cangiano has created a Pygments bundle for TextMate that allows to
colorize code via a simple menu option.  It can be found here_.

.. _here: http://antoniocangiano.com/2008/10/28/pygments-textmate-bundle/

Bash completion
---------------

The source distribution contains a file ``external/pygments.bashcomp`` that
sets up completion for the ``pygmentize`` command in bash.

Wrappers for other languages
----------------------------

These libraries provide Pygments highlighting for users of other languages
than Python:

* `pygments.rb <https://github.com/tmm1/pygments.rb>`_, a pygments wrapper for Ruby
* `Clygments <https://github.com/bfontaine/clygments>`_, a pygments wrapper for
  Clojure
* `PHPygments <https://github.com/capynet/PHPygments>`_, a pygments wrapper for PHP
