# -*- coding: utf-8 -*-
"""
    Basic CLexer Test
    ~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest
import os

from pygments.token import Token
from pygments.lexers import ObjectiveCLexer


class ObjectiveCLexerTest(unittest.TestCase):

    def setUp(self):
        self.lexer = ObjectiveCLexer()

    def testLiteralNumberInt(self):
        fragment = u'@(1);\n'
        expected = [
            (Token.Literal, u'@('),
            (Token.Literal.Number.Integer, u'1'),
            (Token.Literal, u')'),
            (Token.Punctuation, u';'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(expected, list(self.lexer.get_tokens(fragment)))

    def testLiteralNumberExpression(self):
        fragment = u'@(1+2);\n'
        expected = [
            (Token.Literal, u'@('),
            (Token.Literal.Number.Integer, u'1'),
            (Token.Operator, u'+'),
            (Token.Literal.Number.Integer, u'2'),
            (Token.Literal, u')'),
            (Token.Punctuation, u';'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(expected, list(self.lexer.get_tokens(fragment)))

    def testLiteralNumberNestedExpression(self):
        fragment = u'@(1+(2+3));\n'
        expected = [
            (Token.Literal, u'@('),
            (Token.Literal.Number.Integer, u'1'),
            (Token.Operator, u'+'),
            (Token.Punctuation, u'('),
            (Token.Literal.Number.Integer, u'2'),
            (Token.Operator, u'+'),
            (Token.Literal.Number.Integer, u'3'),
            (Token.Punctuation, u')'),
            (Token.Literal, u')'),
            (Token.Punctuation, u';'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(expected, list(self.lexer.get_tokens(fragment)))

    def testLiteralNumberBool(self):
        fragment = u'@NO;\n'
        expected = [
            (Token.Literal.Number, u'@NO'),
            (Token.Punctuation, u';'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(expected, list(self.lexer.get_tokens(fragment)))

    def testLieralNumberBoolExpression(self):
        fragment = u'@(YES);\n'
        expected = [
            (Token.Literal, u'@('),
            (Token.Name.Builtin, u'YES'),
            (Token.Literal, u')'),
            (Token.Punctuation, u';'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(expected, list(self.lexer.get_tokens(fragment)))
