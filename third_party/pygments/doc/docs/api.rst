.. -*- mode: rst -*-

=====================
The full Pygments API
=====================

This page describes the Pygments API.

High-level API
==============

.. module:: pygments

Functions from the :mod:`pygments` module:

.. function:: lex(code, lexer)

    Lex `code` with the `lexer` (must be a `Lexer` instance)
    and return an iterable of tokens. Currently, this only calls
    `lexer.get_tokens()`.

.. function:: format(tokens, formatter, outfile=None)

    Format a token stream (iterable of tokens) `tokens` with the
    `formatter` (must be a `Formatter` instance). The result is
    written to `outfile`, or if that is ``None``, returned as a
    string.

.. function:: highlight(code, lexer, formatter, outfile=None)

    This is the most high-level highlighting function.
    It combines `lex` and `format` in one function.


.. module:: pygments.lexers

Functions from :mod:`pygments.lexers`:

.. function:: get_lexer_by_name(alias, **options)

    Return an instance of a `Lexer` subclass that has `alias` in its
    aliases list. The lexer is given the `options` at its
    instantiation.

    Will raise :exc:`pygments.util.ClassNotFound` if no lexer with that alias is
    found.

.. function:: get_lexer_for_filename(fn, **options)

    Return a `Lexer` subclass instance that has a filename pattern
    matching `fn`. The lexer is given the `options` at its
    instantiation.

    Will raise :exc:`pygments.util.ClassNotFound` if no lexer for that filename
    is found.

.. function:: get_lexer_for_mimetype(mime, **options)

    Return a `Lexer` subclass instance that has `mime` in its mimetype
    list. The lexer is given the `options` at its instantiation.

    Will raise :exc:`pygments.util.ClassNotFound` if not lexer for that mimetype
    is found.

.. function:: guess_lexer(text, **options)

    Return a `Lexer` subclass instance that's guessed from the text in
    `text`. For that, the :meth:`.analyse_text()` method of every known lexer
    class is called with the text as argument, and the lexer which returned the
    highest value will be instantiated and returned.

    :exc:`pygments.util.ClassNotFound` is raised if no lexer thinks it can
    handle the content.

.. function:: guess_lexer_for_filename(filename, text, **options)

    As :func:`guess_lexer()`, but only lexers which have a pattern in `filenames`
    or `alias_filenames` that matches `filename` are taken into consideration.

    :exc:`pygments.util.ClassNotFound` is raised if no lexer thinks it can
    handle the content.

.. function:: get_all_lexers()

    Return an iterable over all registered lexers, yielding tuples in the
    format::

    	(longname, tuple of aliases, tuple of filename patterns, tuple of mimetypes)

    .. versionadded:: 0.6


.. module:: pygments.formatters

Functions from :mod:`pygments.formatters`:

.. function:: get_formatter_by_name(alias, **options)

    Return an instance of a :class:`.Formatter` subclass that has `alias` in its
    aliases list. The formatter is given the `options` at its instantiation.

    Will raise :exc:`pygments.util.ClassNotFound` if no formatter with that
    alias is found.

.. function:: get_formatter_for_filename(fn, **options)

    Return a :class:`.Formatter` subclass instance that has a filename pattern
    matching `fn`. The formatter is given the `options` at its instantiation.

    Will raise :exc:`pygments.util.ClassNotFound` if no formatter for that filename
    is found.


.. module:: pygments.styles

Functions from :mod:`pygments.styles`:

.. function:: get_style_by_name(name)

    Return a style class by its short name. The names of the builtin styles
    are listed in :data:`pygments.styles.STYLE_MAP`.

    Will raise :exc:`pygments.util.ClassNotFound` if no style of that name is
    found.

.. function:: get_all_styles()

    Return an iterable over all registered styles, yielding their names.

    .. versionadded:: 0.6


.. module:: pygments.lexer

Lexers
======

The base lexer class from which all lexers are derived is:

.. class:: Lexer(**options)

    The constructor takes a \*\*keywords dictionary of options.
    Every subclass must first process its own options and then call
    the `Lexer` constructor, since it processes the `stripnl`,
    `stripall` and `tabsize` options.

    An example looks like this:

    .. sourcecode:: python

        def __init__(self, **options):
            self.compress = options.get('compress', '')
            Lexer.__init__(self, **options)

    As these options must all be specifiable as strings (due to the
    command line usage), there are various utility functions
    available to help with that, see `Option processing`_.

    .. method:: get_tokens(text)

        This method is the basic interface of a lexer. It is called by
        the `highlight()` function. It must process the text and return an
        iterable of ``(tokentype, value)`` pairs from `text`.

        Normally, you don't need to override this method. The default
        implementation processes the `stripnl`, `stripall` and `tabsize`
        options and then yields all tokens from `get_tokens_unprocessed()`,
        with the ``index`` dropped.

    .. method:: get_tokens_unprocessed(text)

        This method should process the text and return an iterable of
        ``(index, tokentype, value)`` tuples where ``index`` is the starting
        position of the token within the input text.

        This method must be overridden by subclasses.

    .. staticmethod:: analyse_text(text)

        A static method which is called for lexer guessing. It should analyse
        the text and return a float in the range from ``0.0`` to ``1.0``.
        If it returns ``0.0``, the lexer will not be selected as the most
        probable one, if it returns ``1.0``, it will be selected immediately.

        .. note:: You don't have to add ``@staticmethod`` to the definition of
                  this method, this will be taken care of by the Lexer's metaclass.

    For a list of known tokens have a look at the :doc:`tokens` page.

    A lexer also can have the following attributes (in fact, they are mandatory
    except `alias_filenames`) that are used by the builtin lookup mechanism.

    .. attribute:: name

        Full name for the lexer, in human-readable form.

    .. attribute:: aliases

        A list of short, unique identifiers that can be used to lookup
        the lexer from a list, e.g. using `get_lexer_by_name()`.

    .. attribute:: filenames

        A list of `fnmatch` patterns that match filenames which contain
        content for this lexer. The patterns in this list should be unique among
        all lexers.

    .. attribute:: alias_filenames

        A list of `fnmatch` patterns that match filenames which may or may not
        contain content for this lexer. This list is used by the
        :func:`.guess_lexer_for_filename()` function, to determine which lexers
        are then included in guessing the correct one. That means that
        e.g. every lexer for HTML and a template language should include
        ``\*.html`` in this list.

    .. attribute:: mimetypes

        A list of MIME types for content that can be lexed with this
        lexer.


.. module:: pygments.formatter

Formatters
==========

A formatter is derived from this class:


.. class:: Formatter(**options)

    As with lexers, this constructor processes options and then must call the
    base class :meth:`__init__`.

    The :class:`Formatter` class recognizes the options `style`, `full` and
    `title`.  It is up to the formatter class whether it uses them.

    .. method:: get_style_defs(arg='')

        This method must return statements or declarations suitable to define
        the current style for subsequent highlighted text (e.g. CSS classes
        in the `HTMLFormatter`).

        The optional argument `arg` can be used to modify the generation and
        is formatter dependent (it is standardized because it can be given on
        the command line).

        This method is called by the ``-S`` :doc:`command-line option <cmdline>`,
        the `arg` is then given by the ``-a`` option.

    .. method:: format(tokensource, outfile)

        This method must format the tokens from the `tokensource` iterable and
        write the formatted version to the file object `outfile`.

        Formatter options can control how exactly the tokens are converted.

    .. versionadded:: 0.7
       A formatter must have the following attributes that are used by the
       builtin lookup mechanism.

    .. attribute:: name

        Full name for the formatter, in human-readable form.

    .. attribute:: aliases

        A list of short, unique identifiers that can be used to lookup
        the formatter from a list, e.g. using :func:`.get_formatter_by_name()`.

    .. attribute:: filenames

        A list of :mod:`fnmatch` patterns that match filenames for which this
        formatter can produce output. The patterns in this list should be unique
        among all formatters.


.. module:: pygments.util

Option processing
=================

The :mod:`pygments.util` module has some utility functions usable for option
processing:

.. exception:: OptionError

    This exception will be raised by all option processing functions if
    the type or value of the argument is not correct.

.. function:: get_bool_opt(options, optname, default=None)

    Interpret the key `optname` from the dictionary `options` as a boolean and
    return it. Return `default` if `optname` is not in `options`.

    The valid string values for ``True`` are ``1``, ``yes``, ``true`` and
    ``on``, the ones for ``False`` are ``0``, ``no``, ``false`` and ``off``
    (matched case-insensitively).

.. function:: get_int_opt(options, optname, default=None)

    As :func:`get_bool_opt`, but interpret the value as an integer.

.. function:: get_list_opt(options, optname, default=None)

    If the key `optname` from the dictionary `options` is a string,
    split it at whitespace and return it. If it is already a list
    or a tuple, it is returned as a list.

.. function:: get_choice_opt(options, optname, allowed, default=None)

    If the key `optname` from the dictionary is not in the sequence
    `allowed`, raise an error, otherwise return it.

    .. versionadded:: 0.8
