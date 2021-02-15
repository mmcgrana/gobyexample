# -*- coding: utf-8 -*-
"""
    Basic JavaLexer Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.token import Text, Name, Operator, Keyword, Number
from pygments.lexers import JavaLexer


class JavaTest(unittest.TestCase):

    def setUp(self):
        self.lexer = JavaLexer()
        self.maxDiff = None

    def testEnhancedFor(self):
        fragment = u'label:\nfor(String var2: var1) {}\n'
        tokens = [
            (Name.Label, u'label:'),
            (Text, u'\n'),
            (Keyword, u'for'),
            (Operator, u'('),
            (Name, u'String'),
            (Text, u' '),
            (Name, u'var2'),
            (Operator, u':'),
            (Text, u' '),
            (Name, u'var1'),
            (Operator, u')'),
            (Text, u' '),
            (Operator, u'{'),
            (Operator, u'}'),
            (Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testNumericLiterals(self):
        fragment = '0 5L 9__542_72l 0xbEEf 0X9_A 0_35 01 0b0___101_0'
        fragment += ' 0. .7_17F 3e-1_3d 1f 6_01.9e+3 0x.1Fp3 0XEP8D\n'
        tokens = [
            (Number.Integer, '0'),
            (Text, ' '),
            (Number.Integer, '5L'),
            (Text, ' '),
            (Number.Integer, '9__542_72l'),
            (Text, ' '),
            (Number.Hex, '0xbEEf'),
            (Text, ' '),
            (Number.Hex, '0X9_A'),
            (Text, ' '),
            (Number.Oct, '0_35'),
            (Text, ' '),
            (Number.Oct, '01'),
            (Text, ' '),
            (Number.Bin, '0b0___101_0'),
            (Text, ' '),
            (Number.Float, '0.'),
            (Text, ' '),
            (Number.Float, '.7_17F'),
            (Text, ' '),
            (Number.Float, '3e-1_3d'),
            (Text, ' '),
            (Number.Float, '1f'),
            (Text, ' '),
            (Number.Float, '6_01.9e+3'),
            (Text, ' '),
            (Number.Float, '0x.1Fp3'),
            (Text, ' '),
            (Number.Float, '0XEP8D'),
            (Text, '\n')
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))
