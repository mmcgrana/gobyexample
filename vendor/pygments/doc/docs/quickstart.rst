.. -*- mode: rst -*-

===========================
Introduction and Quickstart
===========================


Welcome to Pygments! This document explains the basic concepts and terms and
gives a few examples of how to use the library.


Architecture
============

There are four types of components that work together highlighting a piece of
code:

* A **lexer** splits the source into tokens, fragments of the source that
  have a token type that determines what the text represents semantically
  (e.g., keyword, string, or comment). There is a lexer for every language
  or markup format that Pygments supports.
* The token stream can be piped through **filters**, which usually modify
  the token types or text fragments, e.g. uppercasing all keywords.
* A **formatter** then takes the token stream and writes it to an output
  file, in a format such as HTML, LaTeX or RTF.
* While writing the output, a **style** determines how to highlight all the
  different token types. It maps them to attributes like "red and bold".


Example
=======

Here is a small example for highlighting Python code:

.. sourcecode:: python

    from pygments import highlight
    from pygments.lexers import PythonLexer
    from pygments.formatters import HtmlFormatter

    code = 'print "Hello World"'
    print highlight(code, PythonLexer(), HtmlFormatter())

which prints something like this:

.. sourcecode:: html

    <div class="highlight">
    <pre><span class="k">print</span> <span class="s">&quot;Hello World&quot;</span></pre>
    </div>

As you can see, Pygments uses CSS classes (by default, but you can change that)
instead of inline styles in order to avoid outputting redundant style information over
and over. A CSS stylesheet that contains all CSS classes possibly used in the output
can be produced by:

.. sourcecode:: python

    print HtmlFormatter().get_style_defs('.highlight')

The argument to :func:`get_style_defs` is used as an additional CSS selector:
the output may look like this:

.. sourcecode:: css

    .highlight .k { color: #AA22FF; font-weight: bold }
    .highlight .s { color: #BB4444 }
    ...


Options
=======

The :func:`highlight()` function supports a fourth argument called *outfile*, it
must be a file object if given. The formatted output will then be written to
this file instead of being returned as a string.

Lexers and formatters both support options. They are given to them as keyword
arguments either to the class or to the lookup method:

.. sourcecode:: python

    from pygments import highlight
    from pygments.lexers import get_lexer_by_name
    from pygments.formatters import HtmlFormatter

    lexer = get_lexer_by_name("python", stripall=True)
    formatter = HtmlFormatter(linenos=True, cssclass="source")
    result = highlight(code, lexer, formatter)

This makes the lexer strip all leading and trailing whitespace from the input
(`stripall` option), lets the formatter output line numbers (`linenos` option),
and sets the wrapping ``<div>``'s class to ``source`` (instead of
``highlight``).

Important options include:

`encoding` : for lexers and formatters
   Since Pygments uses Unicode strings internally, this determines which
   encoding will be used to convert to or from byte strings.
`style` : for formatters
   The name of the style to use when writing the output.


For an overview of builtin lexers and formatters and their options, visit the
:doc:`lexer <lexers>` and :doc:`formatters <formatters>` lists.

For a documentation on filters, see :doc:`this page <filters>`.


Lexer and formatter lookup
==========================

If you want to lookup a built-in lexer by its alias or a filename, you can use
one of the following methods:

.. sourcecode:: pycon

    >>> from pygments.lexers import (get_lexer_by_name,
    ...     get_lexer_for_filename, get_lexer_for_mimetype)

    >>> get_lexer_by_name('python')
    <pygments.lexers.PythonLexer>

    >>> get_lexer_for_filename('spam.rb')
    <pygments.lexers.RubyLexer>

    >>> get_lexer_for_mimetype('text/x-perl')
    <pygments.lexers.PerlLexer>

All these functions accept keyword arguments; they will be passed to the lexer
as options.

A similar API is available for formatters: use :func:`.get_formatter_by_name()`
and :func:`.get_formatter_for_filename()` from the :mod:`pygments.formatters`
module for this purpose.


Guessing lexers
===============

If you don't know the content of the file, or you want to highlight a file
whose extension is ambiguous, such as ``.html`` (which could contain plain HTML
or some template tags), use these functions:

.. sourcecode:: pycon

    >>> from pygments.lexers import guess_lexer, guess_lexer_for_filename

    >>> guess_lexer('#!/usr/bin/python\nprint "Hello World!"')
    <pygments.lexers.PythonLexer>

    >>> guess_lexer_for_filename('test.py', 'print "Hello World!"')
    <pygments.lexers.PythonLexer>

:func:`.guess_lexer()` passes the given content to the lexer classes'
:meth:`analyse_text()` method and returns the one for which it returns the
highest number.

All lexers have two different filename pattern lists: the primary and the
secondary one. The :func:`.get_lexer_for_filename()` function only uses the
primary list, whose entries are supposed to be unique among all lexers.
:func:`.guess_lexer_for_filename()`, however, will first loop through all lexers
and look at the primary and secondary filename patterns if the filename matches.
If only one lexer matches, it is returned, else the guessing mechanism of
:func:`.guess_lexer()` is used with the matching lexers.

As usual, keyword arguments to these functions are given to the created lexer
as options.    


Command line usage
==================

You can use Pygments from the command line, using the :program:`pygmentize`
script::

    $ pygmentize test.py

will highlight the Python file test.py using ANSI escape sequences
(a.k.a. terminal colors) and print the result to standard output.

To output HTML, use the ``-f`` option::

    $ pygmentize -f html -o test.html test.py

to write an HTML-highlighted version of test.py to the file test.html.
Note that it will only be a snippet of HTML, if you want a full HTML document,
use the "full" option::

    $ pygmentize -f html -O full -o test.html test.py

This will produce a full HTML document with included stylesheet.

A style can be selected with ``-O style=<name>``.

If you need a stylesheet for an existing HTML file using Pygments CSS classes,
it can be created with::

    $ pygmentize -S default -f html > style.css

where ``default`` is the style name.

More options and tricks and be found in the :doc:`command line reference
<cmdline>`.
