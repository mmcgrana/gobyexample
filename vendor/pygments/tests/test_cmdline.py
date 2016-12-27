# -*- coding: utf-8 -*-
"""
    Command line test
    ~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from __future__ import print_function

import io
import os
import re
import sys
import tempfile
import unittest

import support
from pygments import cmdline, highlight
from pygments.util import BytesIO, StringIO


TESTFILE, TESTDIR = support.location(__file__)
TESTCODE = '''\
def func(args):
    pass
'''


def run_cmdline(*args, **kwds):
    saved_stdin = sys.stdin
    saved_stdout = sys.stdout
    saved_stderr = sys.stderr
    if sys.version_info > (3,):
        stdin_buffer = BytesIO()
        stdout_buffer = BytesIO()
        stderr_buffer = BytesIO()
        new_stdin = sys.stdin = io.TextIOWrapper(stdin_buffer, 'utf-8')
        new_stdout = sys.stdout = io.TextIOWrapper(stdout_buffer, 'utf-8')
        new_stderr = sys.stderr = io.TextIOWrapper(stderr_buffer, 'utf-8')
    else:
        stdin_buffer = new_stdin = sys.stdin = StringIO()
        stdout_buffer = new_stdout = sys.stdout = StringIO()
        stderr_buffer = new_stderr = sys.stderr = StringIO()
    new_stdin.write(kwds.get('stdin', ''))
    new_stdin.seek(0, 0)
    try:
        ret = cmdline.main(['pygmentize'] + list(args))
    finally:
        sys.stdin = saved_stdin
        sys.stdout = saved_stdout
        sys.stderr = saved_stderr
    new_stdout.flush()
    new_stderr.flush()
    out, err = stdout_buffer.getvalue().decode('utf-8'), \
        stderr_buffer.getvalue().decode('utf-8')
    return (ret, out, err)


class CmdLineTest(unittest.TestCase):

    def check_success(self, *cmdline, **kwds):
        code, out, err = run_cmdline(*cmdline, **kwds)
        self.assertEqual(code, 0)
        self.assertEqual(err, '')
        return out

    def check_failure(self, *cmdline, **kwds):
        expected_code = kwds.pop('code', 1)
        code, out, err = run_cmdline(*cmdline, **kwds)
        self.assertEqual(code, expected_code)
        self.assertEqual(out, '')
        return err

    def test_normal(self):
        # test that cmdline gives the same output as library api
        from pygments.lexers import PythonLexer
        from pygments.formatters import HtmlFormatter
        filename = TESTFILE
        with open(filename, 'rb') as fp:
            code = fp.read()

        output = highlight(code, PythonLexer(), HtmlFormatter())

        o = self.check_success('-lpython', '-fhtml', filename)
        self.assertEqual(o, output)

    def test_stdin(self):
        o = self.check_success('-lpython', '-fhtml', stdin=TESTCODE)
        o = re.sub('<[^>]*>', '', o)
        # rstrip is necessary since HTML inserts a \n after the last </div>
        self.assertEqual(o.rstrip(), TESTCODE.rstrip())

        # guess if no lexer given
        o = self.check_success('-fhtml', stdin=TESTCODE)
        o = re.sub('<[^>]*>', '', o)
        # rstrip is necessary since HTML inserts a \n after the last </div>
        self.assertEqual(o.rstrip(), TESTCODE.rstrip())

    def test_outfile(self):
        # test that output file works with and without encoding
        fd, name = tempfile.mkstemp()
        os.close(fd)
        for opts in [['-fhtml', '-o', name, TESTFILE],
                     ['-flatex', '-o', name, TESTFILE],
                     ['-fhtml', '-o', name, '-O', 'encoding=utf-8', TESTFILE]]:
            try:
                self.check_success(*opts)
            finally:
                os.unlink(name)

    def test_stream_opt(self):
        o = self.check_success('-lpython', '-s', '-fterminal', stdin=TESTCODE)
        o = re.sub(r'\x1b\[.*?m', '', o)
        self.assertEqual(o.replace('\r\n', '\n'), TESTCODE)

    def test_h_opt(self):
        o = self.check_success('-h')
        self.assertTrue('Usage:' in o)

    def test_L_opt(self):
        o = self.check_success('-L')
        self.assertTrue('Lexers' in o and 'Formatters' in o and
                        'Filters' in o and 'Styles' in o)
        o = self.check_success('-L', 'lexer')
        self.assertTrue('Lexers' in o and 'Formatters' not in o)
        self.check_success('-L', 'lexers')

    def test_O_opt(self):
        filename = TESTFILE
        o = self.check_success('-Ofull=1,linenos=true,foo=bar',
                               '-fhtml', filename)
        self.assertTrue('<html' in o)
        self.assertTrue('class="linenos"' in o)

        # "foobar" is invalid for a bool option
        e = self.check_failure('-Ostripnl=foobar', TESTFILE)
        self.assertTrue('Error: Invalid value' in e)
        e = self.check_failure('-Ostripnl=foobar', '-lpy')
        self.assertTrue('Error: Invalid value' in e)

    def test_P_opt(self):
        filename = TESTFILE
        o = self.check_success('-Pfull', '-Ptitle=foo, bar=baz=,',
                               '-fhtml', filename)
        self.assertTrue('<title>foo, bar=baz=,</title>' in o)

    def test_F_opt(self):
        filename = TESTFILE
        o = self.check_success('-Fhighlight:tokentype=Name.Blubb,'
                               'names=TESTFILE filename',
                               '-fhtml', filename)
        self.assertTrue('<span class="n n-Blubb' in o)

    def test_H_opt(self):
        o = self.check_success('-H', 'formatter', 'html')
        self.assertTrue('HTML' in o)
        o = self.check_success('-H', 'lexer', 'python')
        self.assertTrue('Python' in o)
        o = self.check_success('-H', 'filter', 'raiseonerror')
        self.assertTrue('raiseonerror', o)
        e = self.check_failure('-H', 'lexer', 'foobar')
        self.assertTrue('not found' in e)

    def test_S_opt(self):
        o = self.check_success('-S', 'default', '-f', 'html', '-O', 'linenos=1')
        lines = o.splitlines()
        for line in lines:
            # every line is for a token class
            parts = line.split()
            self.assertTrue(parts[0].startswith('.'))
            self.assertTrue(parts[1] == '{')
            if parts[0] != '.hll':
                self.assertTrue(parts[-4] == '}')
                self.assertTrue(parts[-3] == '/*')
                self.assertTrue(parts[-1] == '*/')
        self.check_failure('-S', 'default', '-f', 'foobar')

    def test_N_opt(self):
        o = self.check_success('-N', 'test.py')
        self.assertEqual('python', o.strip())
        o = self.check_success('-N', 'test.unknown')
        self.assertEqual('text', o.strip())

    def test_invalid_opts(self):
        for opts in [
            ('-X',),
            ('-L', '-lpy'),
            ('-L', '-fhtml'),
            ('-L', '-Ox'),
            ('-S', 'default', '-l', 'py', '-f', 'html'),
            ('-S', 'default'),
            ('-a', 'arg'),
            ('-H',),
            (TESTFILE, TESTFILE),
            ('-H', 'formatter'),
            ('-H', 'foo', 'bar'),
            ('-s',),
            ('-s', TESTFILE),
        ]:
            self.check_failure(*opts, code=2)

    def test_errors(self):
        # input file not found
        e = self.check_failure('-lpython', 'nonexistent.py')
        self.assertTrue('Error: cannot read infile' in e)
        self.assertTrue('nonexistent.py' in e)

        # lexer not found
        e = self.check_failure('-lfooo', TESTFILE)
        self.assertTrue('Error: no lexer for alias' in e)

        # formatter not found
        e = self.check_failure('-lpython', '-ffoo', TESTFILE)
        self.assertTrue('Error: no formatter found for name' in e)

        # formatter for outfile not found
        e = self.check_failure('-ofoo.foo', TESTFILE)
        self.assertTrue('Error: no formatter found for file name' in e)

        # output file not writable
        e = self.check_failure('-o', os.path.join('nonexistent', 'dir', 'out.html'),
                               '-lpython', TESTFILE)
        self.assertTrue('Error: cannot open outfile' in e)
        self.assertTrue('out.html' in e)

        # unknown filter
        e = self.check_failure('-F', 'foo', TESTFILE)
        self.assertTrue('Error: filter \'foo\' not found' in e)

    def test_exception(self):
        cmdline.highlight = None  # override callable to provoke TypeError
        try:
            # unexpected exception while highlighting
            e = self.check_failure('-lpython', TESTFILE)
            self.assertTrue('*** Error while highlighting:' in e)
            self.assertTrue('TypeError' in e)

            # same with -v: should reraise the exception
            try:
                self.check_failure('-lpython', '-v', TESTFILE)
            except Exception:
                pass
            else:
                self.fail('exception not reraised')
        finally:
            cmdline.highlight = highlight

    def test_parse_opts(self):
        self.assertEqual(cmdline._parse_options(['  ', 'keyonly,key = value ']),
                         {'keyonly': True, 'key': 'value'})
