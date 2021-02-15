# -*- coding: utf-8 -*-
"""
    Basic Shell Tests
    ~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.token import Token
from pygments.lexers import BashLexer


class BashTest(unittest.TestCase):

    def setUp(self):
        self.lexer = BashLexer()
        self.maxDiff = None

    def testCurlyNoEscapeAndQuotes(self):
        fragment = u'echo "${a//["b"]/}"\n'
        tokens = [
            (Token.Name.Builtin, u'echo'),
            (Token.Text, u' '),
            (Token.Literal.String.Double, u'"'),
            (Token.String.Interpol, u'${'),
            (Token.Name.Variable, u'a'),
            (Token.Punctuation, u'//['),
            (Token.Literal.String.Double, u'"b"'),
            (Token.Punctuation, u']/'),
            (Token.String.Interpol, u'}'),
            (Token.Literal.String.Double, u'"'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testCurlyWithEscape(self):
        fragment = u'echo ${a//[\\"]/}\n'
        tokens = [
            (Token.Name.Builtin, u'echo'),
            (Token.Text, u' '),
            (Token.String.Interpol, u'${'),
            (Token.Name.Variable, u'a'),
            (Token.Punctuation, u'//['),
            (Token.Literal.String.Escape, u'\\"'),
            (Token.Punctuation, u']/'),
            (Token.String.Interpol, u'}'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testParsedSingle(self):
        fragment = u"a=$'abc\\''\n"
        tokens = [
            (Token.Name.Variable, u'a'),
            (Token.Operator, u'='),
            (Token.Literal.String.Single, u"$'abc\\''"),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testShortVariableNames(self):
        fragment = u'x="$"\ny="$_"\nz="$abc"\n'
        tokens = [
            # single lone $
            (Token.Name.Variable, u'x'),
            (Token.Operator, u'='),
            (Token.Literal.String.Double, u'"'),
            (Token.Text, u'$'),
            (Token.Literal.String.Double, u'"'),
            (Token.Text, u'\n'),
            # single letter shell var
            (Token.Name.Variable, u'y'),
            (Token.Operator, u'='),
            (Token.Literal.String.Double, u'"'),
            (Token.Name.Variable, u'$_'),
            (Token.Literal.String.Double, u'"'),
            (Token.Text, u'\n'),
            # multi-letter user var
            (Token.Name.Variable, u'z'),
            (Token.Operator, u'='),
            (Token.Literal.String.Double, u'"'),
            (Token.Name.Variable, u'$abc'),
            (Token.Literal.String.Double, u'"'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))
