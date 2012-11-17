================
Register Plugins
================

If you want to extend Pygments without hacking the sources, but want to
use the lexer/formatter/style/filter lookup functions (`lexers.get_lexer_by_name`
et al.), you can use `setuptools`_ entrypoints to add new lexers, formatters
or styles as if they were in the Pygments core.

.. _setuptools: http://peak.telecommunity.com/DevCenter/setuptools

That means you can use your highlighter modules with the `pygmentize` script,
which relies on the mentioned functions.


Entrypoints
===========

Here is a list of setuptools entrypoints that Pygments understands:

`pygments.lexers`

    This entrypoint is used for adding new lexers to the Pygments core.
    The name of the entrypoint values doesn't really matter, Pygments extracts
    required metadata from the class definition:

    .. sourcecode:: ini

        [pygments.lexers]
        yourlexer = yourmodule:YourLexer

    Note that you have to define ``name``, ``aliases`` and ``filename``
    attributes so that you can use the highlighter from the command line:

    .. sourcecode:: python

        class YourLexer(...):
            name = 'Name Of Your Lexer'
            aliases = ['alias']
            filenames = ['*.ext']


`pygments.formatters`

    You can use this entrypoint to add new formatters to Pygments. The
    name of an entrypoint item is the name of the formatter. If you
    prefix the name with a slash it's used as a filename pattern:

    .. sourcecode:: ini

        [pygments.formatters]
        yourformatter = yourmodule:YourFormatter
        /.ext = yourmodule:YourFormatter


`pygments.styles`

    To add a new style you can use this entrypoint. The name of the entrypoint
    is the name of the style:

    .. sourcecode:: ini

        [pygments.styles]
        yourstyle = yourmodule:YourStyle


`pygments.filters`

    Use this entrypoint to register a new filter. The name of the
    entrypoint is the name of the filter:

    .. sourcecode:: ini

        [pygments.filters]
        yourfilter = yourmodule:YourFilter


How To Use Entrypoints
======================

This documentation doesn't explain how to use those entrypoints because this is
covered in the `setuptools documentation`_. That page should cover everything
you need to write a plugin.

.. _setuptools documentation: http://peak.telecommunity.com/DevCenter/setuptools


Extending The Core
==================

If you have written a Pygments plugin that is open source, please inform us
about that. There is a high chance that we'll add it to the Pygments
distribution.
