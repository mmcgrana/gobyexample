# -*- coding: utf-8 -*-
"""
    Basic Tests for textfmts
    ~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.token import Operator, Number, Text, Token
from pygments.lexers.textfmts import HttpLexer


class RubyTest(unittest.TestCase):

    def setUp(self):
        self.lexer = HttpLexer()
        self.maxDiff = None

    def testApplicationXml(self):
        fragment = u'GET / HTTP/1.0\nContent-Type: application/xml\n\n<foo>\n'
        tokens = [
            (Token.Name.Tag, u'<foo'),
            (Token.Name.Tag, u'>'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(
            tokens, list(self.lexer.get_tokens(fragment))[-len(tokens):])

    def testApplicationCalendarXml(self):
        fragment = u'GET / HTTP/1.0\nContent-Type: application/calendar+xml\n\n<foo>\n'
        tokens = [
            (Token.Name.Tag, u'<foo'),
            (Token.Name.Tag, u'>'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(
            tokens, list(self.lexer.get_tokens(fragment))[-len(tokens):])

