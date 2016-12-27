# -*- coding: utf-8 -*-
"""
    Basic RubyLexer Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.token import Operator, Number, Text, Token
from pygments.lexers import RubyLexer


class RubyTest(unittest.TestCase):

    def setUp(self):
        self.lexer = RubyLexer()
        self.maxDiff = None

    def testRangeSyntax1(self):
        fragment = u'1..3\n'
        tokens = [
            (Number.Integer, u'1'),
            (Operator, u'..'),
            (Number.Integer, u'3'),
            (Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testRangeSyntax2(self):
        fragment = u'1...3\n'
        tokens = [
            (Number.Integer, u'1'),
            (Operator, u'...'),
            (Number.Integer, u'3'),
            (Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testRangeSyntax3(self):
        fragment = u'1 .. 3\n'
        tokens = [
            (Number.Integer, u'1'),
            (Text, u' '),
            (Operator, u'..'),
            (Text, u' '),
            (Number.Integer, u'3'),
            (Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testInterpolationNestedCurly(self):
        fragment = (
            u'"A#{ (3..5).group_by { |x| x/2}.map '
            u'do |k,v| "#{k}" end.join }" + "Z"\n')

        tokens = [
            (Token.Literal.String.Double, u'"'),
            (Token.Literal.String.Double, u'A'),
            (Token.Literal.String.Interpol, u'#{'),
            (Token.Text, u' '),
            (Token.Punctuation, u'('),
            (Token.Literal.Number.Integer, u'3'),
            (Token.Operator, u'..'),
            (Token.Literal.Number.Integer, u'5'),
            (Token.Punctuation, u')'),
            (Token.Operator, u'.'),
            (Token.Name, u'group_by'),
            (Token.Text, u' '),
            (Token.Literal.String.Interpol, u'{'),
            (Token.Text, u' '),
            (Token.Operator, u'|'),
            (Token.Name, u'x'),
            (Token.Operator, u'|'),
            (Token.Text, u' '),
            (Token.Name, u'x'),
            (Token.Operator, u'/'),
            (Token.Literal.Number.Integer, u'2'),
            (Token.Literal.String.Interpol, u'}'),
            (Token.Operator, u'.'),
            (Token.Name, u'map'),
            (Token.Text, u' '),
            (Token.Keyword, u'do'),
            (Token.Text, u' '),
            (Token.Operator, u'|'),
            (Token.Name, u'k'),
            (Token.Punctuation, u','),
            (Token.Name, u'v'),
            (Token.Operator, u'|'),
            (Token.Text, u' '),
            (Token.Literal.String.Double, u'"'),
            (Token.Literal.String.Interpol, u'#{'),
            (Token.Name, u'k'),
            (Token.Literal.String.Interpol, u'}'),
            (Token.Literal.String.Double, u'"'),
            (Token.Text, u' '),
            (Token.Keyword, u'end'),
            (Token.Operator, u'.'),
            (Token.Name, u'join'),
            (Token.Text, u' '),
            (Token.Literal.String.Interpol, u'}'),
            (Token.Literal.String.Double, u'"'),
            (Token.Text, u' '),
            (Token.Operator, u'+'),
            (Token.Text, u' '),
            (Token.Literal.String.Double, u'"'),
            (Token.Literal.String.Double, u'Z'),
            (Token.Literal.String.Double, u'"'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testOperatorMethods(self):
        fragment = u'x.==4\n'
        tokens = [
            (Token.Name, u'x'),
            (Token.Operator, u'.'),
            (Token.Name.Operator, u'=='),
            (Token.Literal.Number.Integer, u'4'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testEscapedBracestring(self):
        fragment = u'str.gsub(%r{\\\\\\\\}, "/")\n'
        tokens = [
            (Token.Name, u'str'),
            (Token.Operator, u'.'),
            (Token.Name, u'gsub'),
            (Token.Punctuation, u'('),
            (Token.Literal.String.Regex, u'%r{'),
            (Token.Literal.String.Regex, u'\\\\'),
            (Token.Literal.String.Regex, u'\\\\'),
            (Token.Literal.String.Regex, u'}'),
            (Token.Punctuation, u','),
            (Token.Text, u' '),
            (Token.Literal.String.Double, u'"'),
            (Token.Literal.String.Double, u'/'),
            (Token.Literal.String.Double, u'"'),
            (Token.Punctuation, u')'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))
