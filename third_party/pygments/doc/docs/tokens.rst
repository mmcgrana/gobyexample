.. -*- mode: rst -*-

==============
Builtin Tokens
==============

.. module:: pygments.token

In the :mod:`pygments.token` module, there is a special object called `Token`
that is used to create token types.

You can create a new token type by accessing an attribute of `Token`:

.. sourcecode:: pycon

    >>> from pygments.token import Token
    >>> Token.String
    Token.String
    >>> Token.String is Token.String
    True

Note that tokens are singletons so you can use the ``is`` operator for comparing
token types.

As of Pygments 0.7 you can also use the ``in`` operator to perform set tests:

.. sourcecode:: pycon

    >>> from pygments.token import Comment
    >>> Comment.Single in Comment
    True
    >>> Comment in Comment.Multi
    False

This can be useful in :doc:`filters <filters>` and if you write lexers on your
own without using the base lexers.

You can also split a token type into a hierarchy, and get the parent of it:

.. sourcecode:: pycon

    >>> String.split()
    [Token, Token.Literal, Token.Literal.String]
    >>> String.parent
    Token.Literal

In principle, you can create an unlimited number of token types but nobody can
guarantee that a style would define style rules for a token type. Because of
that, Pygments proposes some global token types defined in the
`pygments.token.STANDARD_TYPES` dict.

For some tokens aliases are already defined:

.. sourcecode:: pycon

    >>> from pygments.token import String
    >>> String
    Token.Literal.String

Inside the :mod:`pygments.token` module the following aliases are defined:

============= ============================ ====================================
`Text`        `Token.Text`                 for any type of text data
`Whitespace`  `Token.Text.Whitespace`      for specially highlighted whitespace
`Error`       `Token.Error`                represents lexer errors
`Other`       `Token.Other`                special token for data not
                                           matched by a parser (e.g. HTML
                                           markup in PHP code)
`Keyword`     `Token.Keyword`              any kind of keywords
`Name`        `Token.Name`                 variable/function names
`Literal`     `Token.Literal`              Any literals
`String`      `Token.Literal.String`       string literals
`Number`      `Token.Literal.Number`       number literals
`Operator`    `Token.Operator`             operators (``+``, ``not``...)
`Punctuation` `Token.Punctuation`          punctuation (``[``, ``(``...)
`Comment`     `Token.Comment`              any kind of comments
`Generic`     `Token.Generic`              generic tokens (have a look at
                                           the explanation below)
============= ============================ ====================================

The `Whitespace` token type is new in Pygments 0.8. It is used only by the
`VisibleWhitespaceFilter` currently.

Normally you just create token types using the already defined aliases. For each
of those token aliases, a number of subtypes exists (excluding the special tokens
`Token.Text`, `Token.Error` and `Token.Other`)

The `is_token_subtype()` function in the `pygments.token` module can be used to
test if a token type is a subtype of another (such as `Name.Tag` and `Name`).
(This is the same as ``Name.Tag in Name``. The overloaded `in` operator was newly
introduced in Pygments 0.7, the function still exists for backwards
compatibility.)

With Pygments 0.7, it's also possible to convert strings to token types (for example
if you want to supply a token from the command line):

.. sourcecode:: pycon

    >>> from pygments.token import String, string_to_tokentype
    >>> string_to_tokentype("String")
    Token.Literal.String
    >>> string_to_tokentype("Token.Literal.String")
    Token.Literal.String
    >>> string_to_tokentype(String)
    Token.Literal.String


Keyword Tokens
==============

`Keyword`
    For any kind of keyword (especially if it doesn't match any of the
    subtypes of course).

`Keyword.Constant`
    For keywords that are constants (e.g. ``None`` in future Python versions).

`Keyword.Declaration`
    For keywords used for variable declaration (e.g. ``var`` in some programming
    languages like JavaScript).

`Keyword.Namespace`
    For keywords used for namespace declarations (e.g. ``import`` in Python and
    Java and ``package`` in Java).

`Keyword.Pseudo`
    For keywords that aren't really keywords (e.g. ``None`` in old Python
    versions).

`Keyword.Reserved`
    For reserved keywords.

`Keyword.Type`
    For builtin types that can't be used as identifiers (e.g. ``int``,
    ``char`` etc. in C).


Name Tokens
===========

`Name`
    For any name (variable names, function names, classes).

`Name.Attribute`
    For all attributes (e.g. in HTML tags).

`Name.Builtin`
    Builtin names; names that are available in the global namespace.

`Name.Builtin.Pseudo`
    Builtin names that are implicit (e.g. ``self`` in Ruby, ``this`` in Java).

`Name.Class`
    Class names. Because no lexer can know if a name is a class or a function
    or something else this token is meant for class declarations.

`Name.Constant`
    Token type for constants. In some languages you can recognise a token by the
    way it's defined (the value after a ``const`` keyword for example). In
    other languages constants are uppercase by definition (Ruby).

`Name.Decorator`
    Token type for decorators. Decorators are syntactic elements in the Python
    language. Similar syntax elements exist in C# and Java.

`Name.Entity`
    Token type for special entities. (e.g. ``&nbsp;`` in HTML).

`Name.Exception`
    Token type for exception names (e.g. ``RuntimeError`` in Python). Some languages
    define exceptions in the function signature (Java). You can highlight
    the name of that exception using this token then.

`Name.Function`
    Token type for function names.

`Name.Label`
    Token type for label names (e.g. in languages that support ``goto``).

`Name.Namespace`
    Token type for namespaces. (e.g. import paths in Java/Python), names following
    the ``module``/``namespace`` keyword in other languages.

`Name.Other`
    Other names. Normally unused.

`Name.Tag`
    Tag names (in HTML/XML markup or configuration files).

`Name.Variable`
    Token type for variables. Some languages have prefixes for variable names
    (PHP, Ruby, Perl). You can highlight them using this token.

`Name.Variable.Class`
    same as `Name.Variable` but for class variables (also static variables).

`Name.Variable.Global`
    same as `Name.Variable` but for global variables (used in Ruby, for
    example).

`Name.Variable.Instance`
    same as `Name.Variable` but for instance variables.


Literals
========

`Literal`
    For any literal (if not further defined).

`Literal.Date`
    for date literals (e.g. ``42d`` in Boo).


`String`
    For any string literal.

`String.Backtick`
    Token type for strings enclosed in backticks.

`String.Char`
    Token type for single characters (e.g. Java, C).

`String.Doc`
    Token type for documentation strings (for example Python).

`String.Double`
    Double quoted strings.

`String.Escape`
    Token type for escape sequences in strings.

`String.Heredoc`
    Token type for "heredoc" strings (e.g. in Ruby or Perl).

`String.Interpol`
    Token type for interpolated parts in strings (e.g. ``#{foo}`` in Ruby).

`String.Other`
    Token type for any other strings (for example ``%q{foo}`` string constructs
    in Ruby).

`String.Regex`
    Token type for regular expression literals (e.g. ``/foo/`` in JavaScript).

`String.Single`
    Token type for single quoted strings.

`String.Symbol`
    Token type for symbols (e.g. ``:foo`` in LISP or Ruby).


`Number`
    Token type for any number literal.

`Number.Bin`
    Token type for binary literals (e.g. ``0b101010``).

`Number.Float`
    Token type for float literals (e.g. ``42.0``).

`Number.Hex`
    Token type for hexadecimal number literals (e.g. ``0xdeadbeef``).

`Number.Integer`
    Token type for integer literals (e.g. ``42``).

`Number.Integer.Long`
    Token type for long integer literals (e.g. ``42L`` in Python).

`Number.Oct`
    Token type for octal literals.


Operators
=========

`Operator`
    For any punctuation operator (e.g. ``+``, ``-``).

`Operator.Word`
    For any operator that is a word (e.g. ``not``).


Punctuation
===========

.. versionadded:: 0.7

`Punctuation`
    For any punctuation which is not an operator (e.g. ``[``, ``(``...)


Comments
========

`Comment`
    Token type for any comment.

`Comment.Hashbang`
    Token type for hashbang comments (i.e. first lines of files that start with
     ``#!``).

`Comment.Multiline`
    Token type for multiline comments.

`Comment.Preproc`
    Token type for preprocessor comments (also ``<?php``/``<%`` constructs).

`Comment.Single`
    Token type for comments that end at the end of a line (e.g. ``# foo``).

`Comment.Special`
    Special data in comments. For example code tags, author and license
    information, etc.


Generic Tokens
==============

Generic tokens are for special lexers like the `DiffLexer` that doesn't really
highlight a programming language but a patch file.


`Generic`
    A generic, unstyled token. Normally you don't use this token type.

`Generic.Deleted`
    Marks the token value as deleted.

`Generic.Emph`
    Marks the token value as emphasized.

`Generic.Error`
    Marks the token value as an error message.

`Generic.Heading`
    Marks the token value as headline.

`Generic.Inserted`
    Marks the token value as inserted.

`Generic.Output`
    Marks the token value as program output (e.g. for python cli lexer).

`Generic.Prompt`
    Marks the token value as command prompt (e.g. bash lexer).

`Generic.Strong`
    Marks the token value as bold (e.g. for rst lexer).

`Generic.Subheading`
    Marks the token value as subheadline.

`Generic.Traceback`
    Marks the token value as a part of an error traceback.
