# -*- coding: utf-8 -*-
"""
    Pygments HTML formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from __future__ import print_function

import io
import os
import re
import unittest
import tempfile
from os.path import join, dirname, isfile

from pygments.util import StringIO
from pygments.lexers import PythonLexer
from pygments.formatters import HtmlFormatter, NullFormatter
from pygments.formatters.html import escape_html

import support

TESTFILE, TESTDIR = support.location(__file__)

with io.open(TESTFILE, encoding='utf-8') as fp:
    tokensource = list(PythonLexer().get_tokens(fp.read()))


class HtmlFormatterTest(unittest.TestCase):
    def test_correct_output(self):
        hfmt = HtmlFormatter(nowrap=True)
        houtfile = StringIO()
        hfmt.format(tokensource, houtfile)

        nfmt = NullFormatter()
        noutfile = StringIO()
        nfmt.format(tokensource, noutfile)

        stripped_html = re.sub('<.*?>', '', houtfile.getvalue())
        escaped_text = escape_html(noutfile.getvalue())
        self.assertEqual(stripped_html, escaped_text)

    def test_external_css(self):
        # test correct behavior
        # CSS should be in /tmp directory
        fmt1 = HtmlFormatter(full=True, cssfile='fmt1.css', outencoding='utf-8')
        # CSS should be in TESTDIR (TESTDIR is absolute)
        fmt2 = HtmlFormatter(full=True, cssfile=join(TESTDIR, 'fmt2.css'),
                             outencoding='utf-8')
        tfile = tempfile.NamedTemporaryFile(suffix='.html')
        fmt1.format(tokensource, tfile)
        try:
            fmt2.format(tokensource, tfile)
            self.assertTrue(isfile(join(TESTDIR, 'fmt2.css')))
        except IOError:
            # test directory not writable
            pass
        tfile.close()

        self.assertTrue(isfile(join(dirname(tfile.name), 'fmt1.css')))
        os.unlink(join(dirname(tfile.name), 'fmt1.css'))
        try:
            os.unlink(join(TESTDIR, 'fmt2.css'))
        except OSError:
            pass

    def test_all_options(self):
        def check(optdict):
            outfile = StringIO()
            fmt = HtmlFormatter(**optdict)
            fmt.format(tokensource, outfile)

        for optdict in [
            dict(nowrap=True),
            dict(linenos=True, full=True),
            dict(linenos=True, linespans='L'),
            dict(hl_lines=[1, 5, 10, 'xxx']),
            dict(hl_lines=[1, 5, 10], noclasses=True),
        ]:
            check(optdict)

        for linenos in [False, 'table', 'inline']:
            for noclasses in [False, True]:
                for linenospecial in [0, 5]:
                    for anchorlinenos in [False, True]:
                        optdict = dict(
                            linenos=linenos,
                            noclasses=noclasses,
                            linenospecial=linenospecial,
                            anchorlinenos=anchorlinenos,
                        )
                        check(optdict)

    def test_linenos(self):
        optdict = dict(linenos=True)
        outfile = StringIO()
        fmt = HtmlFormatter(**optdict)
        fmt.format(tokensource, outfile)
        html = outfile.getvalue()
        self.assertTrue(re.search("<pre>\s+1\s+2\s+3", html))

    def test_linenos_with_startnum(self):
        optdict = dict(linenos=True, linenostart=5)
        outfile = StringIO()
        fmt = HtmlFormatter(**optdict)
        fmt.format(tokensource, outfile)
        html = outfile.getvalue()
        self.assertTrue(re.search("<pre>\s+5\s+6\s+7", html))

    def test_lineanchors(self):
        optdict = dict(lineanchors="foo")
        outfile = StringIO()
        fmt = HtmlFormatter(**optdict)
        fmt.format(tokensource, outfile)
        html = outfile.getvalue()
        self.assertTrue(re.search("<pre><span></span><a name=\"foo-1\">", html))

    def test_lineanchors_with_startnum(self):
        optdict = dict(lineanchors="foo", linenostart=5)
        outfile = StringIO()
        fmt = HtmlFormatter(**optdict)
        fmt.format(tokensource, outfile)
        html = outfile.getvalue()
        self.assertTrue(re.search("<pre><span></span><a name=\"foo-5\">", html))

    def test_valid_output(self):
        # test all available wrappers
        fmt = HtmlFormatter(full=True, linenos=True, noclasses=True,
                            outencoding='utf-8')

        handle, pathname = tempfile.mkstemp('.html')
        tfile = os.fdopen(handle, 'w+b')
        fmt.format(tokensource, tfile)
        tfile.close()
        catname = os.path.join(TESTDIR, 'dtds', 'HTML4.soc')
        try:
            import subprocess
            po = subprocess.Popen(['nsgmls', '-s', '-c', catname, pathname],
                                  stdout=subprocess.PIPE)
            ret = po.wait()
            output = po.stdout.read()
            po.stdout.close()
        except OSError:
            # nsgmls not available
            pass
        else:
            if ret:
                print(output)
            self.assertFalse(ret, 'nsgmls run reported errors')

        os.unlink(pathname)

    def test_get_style_defs(self):
        fmt = HtmlFormatter()
        sd = fmt.get_style_defs()
        self.assertTrue(sd.startswith('.'))

        fmt = HtmlFormatter(cssclass='foo')
        sd = fmt.get_style_defs()
        self.assertTrue(sd.startswith('.foo'))
        sd = fmt.get_style_defs('.bar')
        self.assertTrue(sd.startswith('.bar'))
        sd = fmt.get_style_defs(['.bar', '.baz'])
        fl = sd.splitlines()[0]
        self.assertTrue('.bar' in fl and '.baz' in fl)

    def test_unicode_options(self):
        fmt = HtmlFormatter(title=u'Föö',
                            cssclass=u'bär',
                            cssstyles=u'div:before { content: \'bäz\' }',
                            encoding='utf-8')
        handle, pathname = tempfile.mkstemp('.html')
        tfile = os.fdopen(handle, 'w+b')
        fmt.format(tokensource, tfile)
        tfile.close()

    def test_ctags(self):
        try:
            import ctags
        except ImportError:
            # we can't check without the ctags module, but at least check the exception
            self.assertRaises(RuntimeError, HtmlFormatter, tagsfile='support/tags')
        else:
            # this tagfile says that test_ctags() is on line 165, even if it isn't
            # anymore in the actual source
            fmt = HtmlFormatter(tagsfile='support/tags', lineanchors='L',
                                tagurlformat='%(fname)s%(fext)s')
            outfile = StringIO()
            fmt.format(tokensource, outfile)
            self.assertTrue('<a href="test_html_formatter.py#L-165">test_ctags</a>'
                            in outfile.getvalue())

    def test_filename(self):
        optdict = dict(filename="test.py")
        outfile = StringIO()
        fmt = HtmlFormatter(**optdict)
        fmt.format(tokensource, outfile)
        html = outfile.getvalue()
        self.assertTrue(re.search("<span class=\"filename\">test.py</span><pre>", html))
