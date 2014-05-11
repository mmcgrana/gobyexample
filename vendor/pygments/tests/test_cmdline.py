# -*- coding: utf-8 -*-
"""
    Command line test
    ~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2013 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

# Test the command line interface

import sys, os
import unittest
import StringIO

from pygments import highlight
from pygments.cmdline import main as cmdline_main

import support

TESTFILE, TESTDIR = support.location(__file__)


def run_cmdline(*args):
    saved_stdout = sys.stdout
    saved_stderr = sys.stderr
    new_stdout = sys.stdout = StringIO.StringIO()
    new_stderr = sys.stderr = StringIO.StringIO()
    try:
        ret = cmdline_main(["pygmentize"] + list(args))
    finally:
        sys.stdout = saved_stdout
        sys.stderr = saved_stderr
    return (ret, new_stdout.getvalue(), new_stderr.getvalue())


class CmdLineTest(unittest.TestCase):

    def test_L_opt(self):
        c, o, e = run_cmdline("-L")
        self.assertEqual(c, 0)
        self.assertTrue("Lexers" in o and "Formatters" in o and
                        "Filters" in o and "Styles" in o)
        c, o, e = run_cmdline("-L", "lexer")
        self.assertEqual(c, 0)
        self.assertTrue("Lexers" in o and "Formatters" not in o)
        c, o, e = run_cmdline("-L", "lexers")
        self.assertEqual(c, 0)

    def test_O_opt(self):
        filename = TESTFILE
        c, o, e = run_cmdline("-Ofull=1,linenos=true,foo=bar",
                              "-fhtml", filename)
        self.assertEqual(c, 0)
        self.assertTrue("<html" in o)
        self.assertTrue('class="linenos"' in o)

    def test_P_opt(self):
        filename = TESTFILE
        c, o, e = run_cmdline("-Pfull", "-Ptitle=foo, bar=baz=,",
                              "-fhtml", filename)
        self.assertEqual(c, 0)
        self.assertTrue("<title>foo, bar=baz=,</title>" in o)

    def test_F_opt(self):
        filename = TESTFILE
        c, o, e = run_cmdline("-Fhighlight:tokentype=Name.Blubb,"
                              "names=TESTFILE filename",
                              "-fhtml", filename)
        self.assertEqual(c, 0)
        self.assertTrue('<span class="n-Blubb' in o)

    def test_H_opt(self):
        c, o, e = run_cmdline("-H", "formatter", "html")
        self.assertEqual(c, 0)
        self.assertTrue('HTML' in o)

    def test_S_opt(self):
        c, o, e = run_cmdline("-S", "default", "-f", "html", "-O", "linenos=1")
        self.assertEqual(c, 0)

    def test_invalid_opts(self):
        for opts in [("-L", "-lpy"), ("-L", "-fhtml"), ("-L", "-Ox"),
                     ("-a",), ("-Sst", "-lpy"), ("-H",),
                     ("-H", "formatter"),]:
            self.assertTrue(run_cmdline(*opts)[0] == 2)

    def test_normal(self):
        # test that cmdline gives the same output as library api
        from pygments.lexers import PythonLexer
        from pygments.formatters import HtmlFormatter
        filename = TESTFILE
        fp = open(filename, 'rb')
        try:
            code = fp.read()
        finally:
            fp.close()

        output = highlight(code, PythonLexer(), HtmlFormatter())

        c, o, e = run_cmdline("-lpython", "-fhtml", filename)

        self.assertEqual(o, output)
        self.assertEqual(e, "")
        self.assertEqual(c, 0)
