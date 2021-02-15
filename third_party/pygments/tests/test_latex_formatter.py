# -*- coding: utf-8 -*-
"""
    Pygments LaTeX formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from __future__ import print_function

import os
import unittest
import tempfile

from pygments.formatters import LatexFormatter
from pygments.lexers import PythonLexer

import support

TESTFILE, TESTDIR = support.location(__file__)


class LatexFormatterTest(unittest.TestCase):

    def test_valid_output(self):
        with open(TESTFILE) as fp:
            tokensource = list(PythonLexer().get_tokens(fp.read()))
        fmt = LatexFormatter(full=True, encoding='latin1')

        handle, pathname = tempfile.mkstemp('.tex')
        # place all output files in /tmp too
        old_wd = os.getcwd()
        os.chdir(os.path.dirname(pathname))
        tfile = os.fdopen(handle, 'wb')
        fmt.format(tokensource, tfile)
        tfile.close()
        try:
            import subprocess
            po = subprocess.Popen(['latex', '-interaction=nonstopmode',
                                   pathname], stdout=subprocess.PIPE)
            ret = po.wait()
            output = po.stdout.read()
            po.stdout.close()
        except OSError:
            # latex not available
            raise support.SkipTest
        else:
            if ret:
                print(output)
            self.assertFalse(ret, 'latex run reported errors')

        os.unlink(pathname)
        os.chdir(old_wd)
