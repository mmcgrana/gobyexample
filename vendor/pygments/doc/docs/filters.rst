.. -*- mode: rst -*-

=======
Filters
=======

.. versionadded:: 0.7

You can filter token streams coming from lexers to improve or annotate the
output. For example, you can highlight special words in comments, convert
keywords to upper or lowercase to enforce a style guide etc.

To apply a filter, you can use the `add_filter()` method of a lexer:

.. sourcecode:: pycon

    >>> from pygments.lexers import PythonLexer
    >>> l = PythonLexer()
    >>> # add a filter given by a string and options
    >>> l.add_filter('codetagify', case='lower')
    >>> l.filters
    [<pygments.filters.CodeTagFilter object at 0xb785decc>]
    >>> from pygments.filters import KeywordCaseFilter
    >>> # or give an instance
    >>> l.add_filter(KeywordCaseFilter(case='lower'))

The `add_filter()` method takes keyword arguments which are forwarded to
the constructor of the filter.

To get a list of all registered filters by name, you can use the
`get_all_filters()` function from the `pygments.filters` module that returns an
iterable for all known filters.

If you want to write your own filter, have a look at :doc:`Write your own filter
<filterdevelopment>`.


Builtin Filters
===============

.. pygmentsdoc:: filters
