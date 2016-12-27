# -*- coding: utf-8 -*-
"""
    Pygments IRC formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from __future__ import print_function

import re
import unittest

from pygments.util import StringIO
from pygments.lexers import PythonLexer
from pygments.formatters import IRCFormatter

import support

tokensource = list(PythonLexer().get_tokens("lambda x: 123"))

class IRCFormatterTest(unittest.TestCase):
    def test_correct_output(self):
        hfmt = IRCFormatter()
        houtfile = StringIO()
        hfmt.format(tokensource, houtfile)

        self.assertEqual(u'\x0302lambda\x03 x: \x0302123\x03\n', houtfile.getvalue())

