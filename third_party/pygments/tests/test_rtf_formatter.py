# -*- coding: utf-8 -*-
"""
    Pygments RTF formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest
from string_asserts import StringTests

from pygments.util import StringIO
from pygments.formatters import RtfFormatter
from pygments.lexers.special import TextLexer

class RtfFormatterTest(StringTests, unittest.TestCase):
    foot = (r'\par' '\n' r'}')

    def _escape(self, string):
        return(string.replace("\n", r"\n"))

    def _build_message(self, *args, **kwargs):
        string = kwargs.get('string', None)
        t = self._escape(kwargs.get('t', ''))
        expected = self._escape(kwargs.get('expected', ''))
        result = self._escape(kwargs.get('result', ''))

        if string is None:
            string = (u"The expected output of '{t}'\n"
                      u"\t\tShould be '{expected}'\n"
                      u"\t\tActually outputs '{result}'\n"
                      u"\t(WARNING: Partial Output of Result!)")

        end = -(len(self._escape(self.foot)))
        start = end-len(expected)

        return string.format(t=t,
                             result = result[start:end],
                             expected = expected)

    def format_rtf(self, t):
        tokensource = list(TextLexer().get_tokens(t))
        fmt = RtfFormatter()
        buf = StringIO()
        fmt.format(tokensource, buf)
        result = buf.getvalue()
        buf.close()
        return result

    def test_rtf_header(self):
        t = u''
        result = self.format_rtf(t)
        expected = r'{\rtf1\ansi\uc0'
        msg = (u"RTF documents are expected to start with '{expected}'\n"
               u"\t\tStarts intead with '{result}'\n"
               u"\t(WARNING: Partial Output of Result!)".format(
                   expected = expected,
                   result = result[:len(expected)]))
        self.assertStartsWith(result, expected, msg)

    def test_rtf_footer(self):
        t = u''
        result = self.format_rtf(t)
        expected = self.foot
        msg = (u"RTF documents are expected to end with '{expected}'\n"
               u"\t\tEnds intead with '{result}'\n"
               u"\t(WARNING: Partial Output of Result!)".format(
                   expected = self._escape(expected),
                   result = self._escape(result[-len(expected):])))
        self.assertEndsWith(result, expected, msg)

    def test_ascii_characters(self):
        t = u'a b c d ~'
        result = self.format_rtf(t)
        expected = (r'a b c d ~')
        if not result.endswith(self.foot):
            return(unittest.skip('RTF Footer incorrect'))
        msg = self._build_message(t=t, result=result, expected=expected)
        self.assertEndsWith(result, expected+self.foot, msg)

    def test_escape_characters(self):
        t = u'\ {{'
        result = self.format_rtf(t)
        expected = (r'\\ \{\{')
        if not result.endswith(self.foot):
            return(unittest.skip('RTF Footer incorrect'))
        msg = self._build_message(t=t, result=result, expected=expected)
        self.assertEndsWith(result, expected+self.foot, msg)

    def test_single_characters(self):
        t = u'â € ¤ каждой'
        result = self.format_rtf(t)
        expected = (r'{\u226} {\u8364} {\u164} '
                    r'{\u1082}{\u1072}{\u1078}{\u1076}{\u1086}{\u1081}')
        if not result.endswith(self.foot):
            return(unittest.skip('RTF Footer incorrect'))
        msg = self._build_message(t=t, result=result, expected=expected)
        self.assertEndsWith(result, expected+self.foot, msg)

    def test_double_characters(self):
        t = u'က 힣 ↕ ↕︎ 鼖'
        result = self.format_rtf(t)
        expected = (r'{\u4096} {\u55203} {\u8597} '
                    r'{\u8597}{\u65038} {\u55422}{\u56859}')
        if not result.endswith(self.foot):
            return(unittest.skip('RTF Footer incorrect'))
        msg = self._build_message(t=t, result=result, expected=expected)
        self.assertEndsWith(result, expected+self.foot, msg)
