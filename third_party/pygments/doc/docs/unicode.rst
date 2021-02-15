=====================
Unicode and Encodings
=====================

Since Pygments 0.6, all lexers use unicode strings internally. Because of that
you might encounter the occasional :exc:`UnicodeDecodeError` if you pass strings
with the wrong encoding.

Per default all lexers have their input encoding set to `guess`.  This means
that the following encodings are tried:

* UTF-8 (including BOM handling)
* The locale encoding (i.e. the result of `locale.getpreferredencoding()`)
* As a last resort, `latin1`

If you pass a lexer a byte string object (not unicode), it tries to decode the
data using this encoding.

You can override the encoding using the `encoding` or `inencoding` lexer
options.  If you have the `chardet`_ library installed and set the encoding to
``chardet`` if will analyse the text and use the encoding it thinks is the
right one automatically:

.. sourcecode:: python

    from pygments.lexers import PythonLexer
    lexer = PythonLexer(encoding='chardet')

The best way is to pass Pygments unicode objects. In that case you can't get
unexpected output.

The formatters now send Unicode objects to the stream if you don't set the
output encoding. You can do so by passing the formatters an `encoding` option:

.. sourcecode:: python

    from pygments.formatters import HtmlFormatter
    f = HtmlFormatter(encoding='utf-8')

**You will have to set this option if you have non-ASCII characters in the
source and the output stream does not accept Unicode written to it!**
This is the case for all regular files and for terminals.

Note: The Terminal formatter tries to be smart: if its output stream has an
`encoding` attribute, and you haven't set the option, it will encode any
Unicode string with this encoding before writing it. This is the case for
`sys.stdout`, for example. The other formatters don't have that behavior.

Another note: If you call Pygments via the command line (`pygmentize`),
encoding is handled differently, see :doc:`the command line docs <cmdline>`.

.. versionadded:: 0.7
   The formatters now also accept an `outencoding` option which will override
   the `encoding` option if given. This makes it possible to use a single
   options dict with lexers and formatters, and still have different input and
   output encodings.

.. _chardet: http://chardet.feedparser.org/
