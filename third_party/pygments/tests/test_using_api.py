# -*- coding: utf-8 -*-
"""
    Pygments tests for using()
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.lexer import using, bygroups, this, RegexLexer
from pygments.token import String, Text, Keyword

class TestLexer(RegexLexer):
    tokens = {
        'root': [
            (r'#.*',
             using(this, state='invalid')),
            (r'(")(.+?)(")',
             bygroups(String, using(this, state='string'), String)),
            (r'[^"]+', Text),
        ],
        'string': [
            (r'.+', Keyword),
        ],
    }


class UsingStateTest(unittest.TestCase):
    def test_basic(self):
        expected = [(Text, 'a'), (String, '"'), (Keyword, 'bcd'),
                    (String, '"'), (Text, 'e\n')]
        t = list(TestLexer().get_tokens('a"bcd"e'))
        self.assertEqual(t, expected)

    def test_error(self):
        def gen():
            return list(TestLexer().get_tokens('#a'))
        self.assertRaises(KeyError, gen)
