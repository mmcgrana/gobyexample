.. -*- mode: rst -*-

====================
Available formatters
====================

This page lists all builtin formatters.

Common options
==============

All formatters support these options:

`encoding`
    If given, must be an encoding name (such as ``"utf-8"``). This will
    be used to convert the token strings (which are Unicode strings)
    to byte strings in the output (default: ``None``).
    It will also be written in an encoding declaration suitable for the
    document format if the `full` option is given (e.g. a ``meta
    content-type`` directive in HTML or an invocation of the `inputenc`
    package in LaTeX).

    If this is ``""`` or ``None``, Unicode strings will be written
    to the output file, which most file-like objects do not support.
    For example, `pygments.highlight()` will return a Unicode string if
    called with no `outfile` argument and a formatter that has `encoding`
    set to ``None`` because it uses a `StringIO.StringIO` object that
    supports Unicode arguments to `write()`. Using a regular file object
    wouldn't work.

    .. versionadded:: 0.6

`outencoding`
    When using Pygments from the command line, any `encoding` option given is
    passed to the lexer and the formatter. This is sometimes not desirable,
    for example if you want to set the input encoding to ``"guess"``.
    Therefore, `outencoding` has been introduced which overrides `encoding`
    for the formatter if given.

    .. versionadded:: 0.7


Formatter classes
=================

All these classes are importable from :mod:`pygments.formatters`.

.. pygmentsdoc:: formatters
