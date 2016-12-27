# -*- coding: utf-8 -*-
"""
    Pygments terminal formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from __future__ import print_function

import unittest
import re

from pygments.util import StringIO
from pygments.lexers.sql import PlPgsqlLexer
from pygments.formatters import TerminalFormatter

DEMO_TEXT = '''\
-- comment
select
* from bar;
'''
DEMO_LEXER = PlPgsqlLexer
DEMO_TOKENS = list(DEMO_LEXER().get_tokens(DEMO_TEXT))

ANSI_RE = re.compile(r'\x1b[\w\W]*?m')

def strip_ansi(x):
    return ANSI_RE.sub('', x)

class TerminalFormatterTest(unittest.TestCase):
    def test_reasonable_output(self):
        out = StringIO()
        TerminalFormatter().format(DEMO_TOKENS, out)
        plain = strip_ansi(out.getvalue())
        self.assertEqual(DEMO_TEXT.count('\n'), plain.count('\n'))
        print(repr(plain))

        for a, b in zip(DEMO_TEXT.splitlines(), plain.splitlines()):
            self.assertEqual(a, b)

    def test_reasonable_output_lineno(self):
        out = StringIO()
        TerminalFormatter(linenos=True).format(DEMO_TOKENS, out)
        plain = strip_ansi(out.getvalue())
        self.assertEqual(DEMO_TEXT.count('\n') + 1, plain.count('\n'))
        print(repr(plain))

        for a, b in zip(DEMO_TEXT.splitlines(), plain.splitlines()):
            self.assertTrue(a in b)
