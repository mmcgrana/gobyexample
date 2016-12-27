.. -*- mode: rst -*-

================================
Using Pygments in ReST documents
================================

Many Python people use `ReST`_ for documentation their sourcecode, programs,
scripts et cetera. This also means that documentation often includes sourcecode
samples or snippets.

You can easily enable Pygments support for your ReST texts using a custom
directive -- this is also how this documentation displays source code.

From Pygments 0.9, the directive is shipped in the distribution as
`external/rst-directive.py`.  You can copy and adapt this code to your liking.

.. removed -- too confusing
   *Loosely related note:* The ReST lexer now recognizes ``.. sourcecode::`` and
   ``.. code::`` directives and highlights the contents in the specified language
   if the `handlecodeblocks` option is true.

.. _ReST: http://docutils.sf.net/rst.html
