.. -*- mode: rst -*-

============================
Using Pygments with MoinMoin
============================

From Pygments 0.7, the source distribution ships a `Moin`_ parser plugin that
can be used to get Pygments highlighting in Moin wiki pages.

To use it, copy the file `external/moin-parser.py` from the Pygments
distribution to the `data/plugin/parser` subdirectory of your Moin instance.
Edit the options at the top of the file (currently ``ATTACHMENTS`` and
``INLINESTYLES``) and rename the file to the name that the parser directive
should have. For example, if you name the file ``code.py``, you can get a
highlighted Python code sample with this Wiki markup::

    {{{
    #!code python
    [...]
    }}}

where ``python`` is the Pygments name of the lexer to use.

Additionally, if you set the ``ATTACHMENTS`` option to True, Pygments will also
be called for all attachments for whose filenames there is no other parser
registered.

You are responsible for including CSS rules that will map the Pygments CSS
classes to colors. You can output a stylesheet file with `pygmentize`, put it
into the `htdocs` directory of your Moin instance and then include it in the
`stylesheets` configuration option in the Moin config, e.g.::

    stylesheets = [('screen', '/htdocs/pygments.css')]

If you do not want to do that and are willing to accept larger HTML output, you
can set the ``INLINESTYLES`` option to True.


.. _Moin: http://moinmoin.wikiwikiweb.de/
