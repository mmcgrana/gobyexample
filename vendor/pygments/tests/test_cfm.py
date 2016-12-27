# -*- coding: utf-8 -*-
"""
    Basic ColdfusionHtmlLexer Test
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest
import os

from pygments.token import Token
from pygments.lexers import ColdfusionHtmlLexer


class ColdfusionHtmlLexerTest(unittest.TestCase):

    def setUp(self):
        self.lexer = ColdfusionHtmlLexer()

    def testBasicComment(self):
        fragment = u'<!--- cfcomment --->'
        expected = [
            (Token.Text, u''),
            (Token.Comment.Multiline, u'<!---'),
            (Token.Comment.Multiline, u' cfcomment '),
            (Token.Comment.Multiline, u'--->'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(expected, list(self.lexer.get_tokens(fragment)))

    def testNestedComment(self):
        fragment = u'<!--- nested <!--- cfcomment ---> --->'
        expected = [
            (Token.Text, u''),
            (Token.Comment.Multiline, u'<!---'),
            (Token.Comment.Multiline, u' nested '),
            (Token.Comment.Multiline, u'<!---'),
            (Token.Comment.Multiline, u' cfcomment '),
            (Token.Comment.Multiline, u'--->'),
            (Token.Comment.Multiline, u' '),
            (Token.Comment.Multiline, u'--->'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(expected, list(self.lexer.get_tokens(fragment)))
