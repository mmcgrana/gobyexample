# -*- coding: utf-8 -*-
"""
    Basic EzhilLexer Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2015 Muthiah Annamalai <ezhillang@gmail.com>
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.token import Operator, Number, Text, Token
from pygments.lexers import EzhilLexer


class EzhilTest(unittest.TestCase):

    def setUp(self):
        self.lexer = EzhilLexer()
        self.maxDiff = None
    
    def testSum(self):
        fragment = u'1+3\n'
        tokens = [
            (Number.Integer, u'1'),
            (Operator, u'+'),
            (Number.Integer, u'3'),
            (Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))
        
    def testGCDExpr(self):
        fragment = u'1^3+(5-5)*gcd(a,b)\n'
        tokens = [
            (Token.Number.Integer,u'1'),
            (Token.Operator,u'^'),
            (Token.Literal.Number.Integer, u'3'),
            (Token.Operator, u'+'),
            (Token.Punctuation, u'('),
            (Token.Literal.Number.Integer, u'5'),
            (Token.Operator, u'-'),
            (Token.Literal.Number.Integer, u'5'),
            (Token.Punctuation, u')'),
            (Token.Operator, u'*'),
            (Token.Name, u'gcd'),
            (Token.Punctuation, u'('),
            (Token.Name, u'a'),
            (Token.Operator, u','),
            (Token.Name, u'b'),
            (Token.Punctuation, u')'),
            (Token.Text, u'\n')
            ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testIfStatement(self):
        fragment = u"""@( 0 > 3 ) ஆனால்
	பதிப்பி "wont print"	
முடி"""
        tokens = [          
            (Token.Operator, u'@'),
            (Token.Punctuation, u'('),
            (Token.Text, u' '),
            (Token.Literal.Number.Integer,u'0'),
            (Token.Text, u' '),
            (Token.Operator,u'>'),
            (Token.Text, u' '),
            (Token.Literal.Number.Integer, u'3'),
            (Token.Text, u' '),
            (Token.Punctuation, u')'),
            (Token.Text, u' '),
            (Token.Keyword, u'ஆனால்'),
            (Token.Text, u'\n'),
            (Token.Text, u'\t'),
            (Token.Keyword, u'பதிப்பி'),
            (Token.Text, u' '),
            (Token.Literal.String, u'"wont print"'),
            (Token.Text, u'\t'),
            (Token.Text, u'\n'),
            (Token.Keyword, u'முடி'),
            (Token.Text, u'\n')
            ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testFunction(self):
        fragment = u"""# (C) முத்தையா அண்ணாமலை 2013, 2015
நிரல்பாகம்  gcd ( x, y )
    மு = max(x,y)
     q = min(x,y)

    @( q == 0 ) ஆனால்
           பின்கொடு  மு
    முடி
    பின்கொடு  gcd( மு - q , q )
முடி\n"""
        tokens = [
            (Token.Comment.Single,
             u'# (C) \u0bae\u0bc1\u0ba4\u0bcd\u0ba4\u0bc8\u0baf\u0bbe \u0b85\u0ba3\u0bcd\u0ba3\u0bbe\u0bae\u0bb2\u0bc8 2013, 2015\n'),
            (Token.Keyword,u'நிரல்பாகம்'),
            (Token.Text, u'  '),
            (Token.Name, u'gcd'),
            (Token.Text, u' '),
            (Token.Punctuation, u'('),
            (Token.Text, u' '),
            (Token.Name, u'x'),
            (Token.Operator, u','),
            (Token.Text, u' '),
            (Token.Name, u'y'),
            (Token.Text, u' '),
            (Token.Punctuation, u')'),
            (Token.Text, u'\n'),
            (Token.Text, u'    '),
            (Token.Name, u'\u0bae\u0bc1'),
            (Token.Text, u' '),
            (Token.Operator, u'='),
            (Token.Text, u' '),
            (Token.Name.Builtin, u'max'),
            (Token.Punctuation, u'('),
            (Token.Name, u'x'),
            (Token.Operator, u','),
            (Token.Name, u'y'),
            (Token.Punctuation, u')'),
            (Token.Text, u'\n'),
            (Token.Text, u'     '),
            (Token.Name, u'q'),
            (Token.Text, u' '),
            (Token.Operator, u'='),
            (Token.Text, u' '),
            (Token.Name.Builtin, u'min'),
            (Token.Punctuation, u'('),
            (Token.Name, u'x'),
            (Token.Operator, u','),
            (Token.Name, u'y'),
            (Token.Punctuation, u')'),
            (Token.Text, u'\n'),
            (Token.Text, u'\n'),
            (Token.Text, u'    '),
            (Token.Operator, u'@'),
            (Token.Punctuation, u'('),
            (Token.Text, u' '),
            (Token.Name, u'q'),
            (Token.Text, u' '),
            (Token.Operator, u'=='),
            (Token.Text, u' '),
            (Token.Literal.Number.Integer, u'0'),
            (Token.Text, u' '),
            (Token.Punctuation, u')'),
            (Token.Text, u' '),
            (Token.Keyword, u'ஆனால்'),
            (Token.Text, u'\n'),
            (Token.Text, u'           '),
            (Token.Keyword, u'பின்கொடு'),
            (Token.Text, u'  '),
            (Token.Name, u'\u0bae\u0bc1'),
            (Token.Text, u'\n'),
            (Token.Text, u'    '),
            (Token.Keyword, u'முடி'),
            (Token.Text, u'\n'),
            (Token.Text, u'    '),
            (Token.Keyword, u'\u0baa\u0bbf\u0ba9\u0bcd\u0b95\u0bca\u0b9f\u0bc1'),
            (Token.Text, u'  '),
            (Token.Name, u'gcd'),
            (Token.Punctuation, u'('),
            (Token.Text, u' '),
            (Token.Name, u'\u0bae\u0bc1'),
            (Token.Text, u' '),
            (Token.Operator, u'-'),
            (Token.Text, u' '),
            (Token.Name, u'q'),
            (Token.Text, u' '),
            (Token.Operator, u','),
            (Token.Text, u' '),
            (Token.Name, u'q'),
            (Token.Text, u' '),
            (Token.Punctuation, u')'),
            (Token.Text, u'\n'),
            (Token.Keyword, u'முடி'), #u'\u0bae\u0bc1\u0b9f\u0bbf'),
            (Token.Text, u'\n')
            ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))
        
if __name__ == "__main__":
    unittest.main()
