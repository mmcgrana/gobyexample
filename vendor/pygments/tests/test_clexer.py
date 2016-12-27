# -*- coding: utf-8 -*-
"""
    Basic CLexer Test
    ~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest
import os
import textwrap

from pygments.token import Text, Number, Token
from pygments.lexers import CLexer


class CLexerTest(unittest.TestCase):

    def setUp(self):
        self.lexer = CLexer()

    def testNumbers(self):
        code = '42 23.42 23. .42 023 0xdeadbeef 23e+42 42e-23'
        wanted = []
        for item in zip([Number.Integer, Number.Float, Number.Float,
                         Number.Float, Number.Oct, Number.Hex,
                         Number.Float, Number.Float], code.split()):
            wanted.append(item)
            wanted.append((Text, ' '))
        wanted = wanted[:-1] + [(Text, '\n')]
        self.assertEqual(list(self.lexer.get_tokens(code)), wanted)

    def testSwitch(self):
        fragment = u'''\
        int main()
        {
            switch (0)
            {
                case 0:
                default:
                    ;
            }
        }
        '''
        tokens = [
            (Token.Keyword.Type, u'int'),
            (Token.Text, u' '),
            (Token.Name.Function, u'main'),
            (Token.Punctuation, u'('),
            (Token.Punctuation, u')'),
            (Token.Text, u'\n'),
            (Token.Punctuation, u'{'),
            (Token.Text, u'\n'),
            (Token.Text, u'    '),
            (Token.Keyword, u'switch'),
            (Token.Text, u' '),
            (Token.Punctuation, u'('),
            (Token.Literal.Number.Integer, u'0'),
            (Token.Punctuation, u')'),
            (Token.Text, u'\n'),
            (Token.Text, u'    '),
            (Token.Punctuation, u'{'),
            (Token.Text, u'\n'),
            (Token.Text, u'        '),
            (Token.Keyword, u'case'),
            (Token.Text, u' '),
            (Token.Literal.Number.Integer, u'0'),
            (Token.Operator, u':'),
            (Token.Text, u'\n'),
            (Token.Text, u'        '),
            (Token.Keyword, u'default'),
            (Token.Operator, u':'),
            (Token.Text, u'\n'),
            (Token.Text, u'            '),
            (Token.Punctuation, u';'),
            (Token.Text, u'\n'),
            (Token.Text, u'    '),
            (Token.Punctuation, u'}'),
            (Token.Text, u'\n'),
            (Token.Punctuation, u'}'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(textwrap.dedent(fragment))))

    def testSwitchSpaceBeforeColon(self):
        fragment = u'''\
        int main()
        {
            switch (0)
            {
                case 0 :
                default :
                    ;
            }
        }
        '''
        tokens = [
            (Token.Keyword.Type, u'int'),
            (Token.Text, u' '),
            (Token.Name.Function, u'main'),
            (Token.Punctuation, u'('),
            (Token.Punctuation, u')'),
            (Token.Text, u'\n'),
            (Token.Punctuation, u'{'),
            (Token.Text, u'\n'),
            (Token.Text, u'    '),
            (Token.Keyword, u'switch'),
            (Token.Text, u' '),
            (Token.Punctuation, u'('),
            (Token.Literal.Number.Integer, u'0'),
            (Token.Punctuation, u')'),
            (Token.Text, u'\n'),
            (Token.Text, u'    '),
            (Token.Punctuation, u'{'),
            (Token.Text, u'\n'),
            (Token.Text, u'        '),
            (Token.Keyword, u'case'),
            (Token.Text, u' '),
            (Token.Literal.Number.Integer, u'0'),
            (Token.Text, u' '),
            (Token.Operator, u':'),
            (Token.Text, u'\n'),
            (Token.Text, u'        '),
            (Token.Keyword, u'default'),
            (Token.Text, u' '),
            (Token.Operator, u':'),
            (Token.Text, u'\n'),
            (Token.Text, u'            '),
            (Token.Punctuation, u';'),
            (Token.Text, u'\n'),
            (Token.Text, u'    '),
            (Token.Punctuation, u'}'),
            (Token.Text, u'\n'),
            (Token.Punctuation, u'}'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(textwrap.dedent(fragment))))

    def testLabel(self):
        fragment = u'''\
        int main()
        {
        foo:
          goto foo;
        }
        '''
        tokens = [
            (Token.Keyword.Type, u'int'),
            (Token.Text, u' '),
            (Token.Name.Function, u'main'),
            (Token.Punctuation, u'('),
            (Token.Punctuation, u')'),
            (Token.Text, u'\n'),
            (Token.Punctuation, u'{'),
            (Token.Text, u'\n'),
            (Token.Name.Label, u'foo'),
            (Token.Punctuation, u':'),
            (Token.Text, u'\n'),
            (Token.Text, u'  '),
            (Token.Keyword, u'goto'),
            (Token.Text, u' '),
            (Token.Name, u'foo'),
            (Token.Punctuation, u';'),
            (Token.Text, u'\n'),
            (Token.Punctuation, u'}'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(textwrap.dedent(fragment))))

    def testLabelSpaceBeforeColon(self):
        fragment = u'''\
        int main()
        {
        foo :
          goto foo;
        }
        '''
        tokens = [
            (Token.Keyword.Type, u'int'),
            (Token.Text, u' '),
            (Token.Name.Function, u'main'),
            (Token.Punctuation, u'('),
            (Token.Punctuation, u')'),
            (Token.Text, u'\n'),
            (Token.Punctuation, u'{'),
            (Token.Text, u'\n'),
            (Token.Name.Label, u'foo'),
            (Token.Text, u' '),
            (Token.Punctuation, u':'),
            (Token.Text, u'\n'),
            (Token.Text, u'  '),
            (Token.Keyword, u'goto'),
            (Token.Text, u' '),
            (Token.Name, u'foo'),
            (Token.Punctuation, u';'),
            (Token.Text, u'\n'),
            (Token.Punctuation, u'}'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(textwrap.dedent(fragment))))

    def testLabelFollowedByStatement(self):
        fragment = u'''\
        int main()
        {
        foo:return 0;
          goto foo;
        }
        '''
        tokens = [
            (Token.Keyword.Type, u'int'),
            (Token.Text, u' '),
            (Token.Name.Function, u'main'),
            (Token.Punctuation, u'('),
            (Token.Punctuation, u')'),
            (Token.Text, u'\n'),
            (Token.Punctuation, u'{'),
            (Token.Text, u'\n'),
            (Token.Name.Label, u'foo'),
            (Token.Punctuation, u':'),
            (Token.Keyword, u'return'),
            (Token.Text, u' '),
            (Token.Literal.Number.Integer, u'0'),
            (Token.Punctuation, u';'),
            (Token.Text, u'\n'),
            (Token.Text, u'  '),
            (Token.Keyword, u'goto'),
            (Token.Text, u' '),
            (Token.Name, u'foo'),
            (Token.Punctuation, u';'),
            (Token.Text, u'\n'),
            (Token.Punctuation, u'}'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(textwrap.dedent(fragment))))

    def testPreprocFile(self):
        fragment = u'#include <foo>\n'
        tokens = [
            (Token.Comment.Preproc, u'#'),
            (Token.Comment.Preproc, u'include'),
            (Token.Text, u' '),
            (Token.Comment.PreprocFile, u'<foo>'),
            (Token.Comment.Preproc, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testPreprocFile2(self):
        fragment = u'#include "foo.h"\n'
        tokens = [
            (Token.Comment.Preproc, u'#'),
            (Token.Comment.Preproc, u'include'),
            (Token.Text, u' '),
            (Token.Comment.PreprocFile, u'"foo.h"'),
            (Token.Comment.Preproc, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

