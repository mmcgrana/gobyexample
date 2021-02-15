# -*- coding: utf-8 -*-
"""
    Basic SmartyLexer Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.token import Operator, Number, Text, Token
from pygments.lexers import SmartyLexer


class SmartyTest(unittest.TestCase):

    def setUp(self):
        self.lexer = SmartyLexer()

    def testNestedCurly(self):
        fragment = u'{templateFunction param={anotherFunction} param2=$something}\n'
        tokens = [
            (Token.Comment.Preproc, u'{'),
            (Token.Name.Function, u'templateFunction'),
            (Token.Text, u' '),
            (Token.Name.Attribute, u'param'),
            (Token.Operator, u'='),
            (Token.Comment.Preproc, u'{'),
            (Token.Name.Attribute, u'anotherFunction'),
            (Token.Comment.Preproc, u'}'),
            (Token.Text, u' '),
            (Token.Name.Attribute, u'param2'),
            (Token.Operator, u'='),
            (Token.Name.Variable, u'$something'),
            (Token.Comment.Preproc, u'}'),
            (Token.Other, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

