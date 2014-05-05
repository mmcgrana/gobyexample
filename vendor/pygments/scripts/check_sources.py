#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    Checker for file headers
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Make sure each Python file has a correct file header
    including copyright and license information.

    :copyright: Copyright 2006-2013 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import sys, os, re
import getopt
import cStringIO
from os.path import join, splitext, abspath


checkers = {}

def checker(*suffixes, **kwds):
    only_pkg = kwds.pop('only_pkg', False)
    def deco(func):
        for suffix in suffixes:
            checkers.setdefault(suffix, []).append(func)
        func.only_pkg = only_pkg
        return func
    return deco


name_mail_re = r'[\w ]+(<.*?>)?'
copyright_re = re.compile(r'^    :copyright: Copyright 2006-2013 by '
                          r'the Pygments team, see AUTHORS\.$', re.UNICODE)
copyright_2_re = re.compile(r'^                %s(, %s)*[,.]$' %
                            (name_mail_re, name_mail_re), re.UNICODE)
coding_re    = re.compile(r'coding[:=]\s*([-\w.]+)')
not_ix_re    = re.compile(r'\bnot\s+\S+?\s+i[sn]\s\S+')
is_const_re  = re.compile(r'if.*?==\s+(None|False|True)\b')

misspellings = ["developement", "adress", "verificate",  # ALLOW-MISSPELLING
                "informations"]                          # ALLOW-MISSPELLING


@checker('.py')
def check_syntax(fn, lines):
    try:
        compile(''.join(lines), fn, "exec")
    except SyntaxError, err:
        yield 0, "not compilable: %s" % err


@checker('.py')
def check_style_and_encoding(fn, lines):
    encoding = 'ascii'
    for lno, line in enumerate(lines):
        if len(line) > 90:
            yield lno+1, "line too long"
        m = not_ix_re.search(line)
        if m:
            yield lno+1, '"' + m.group() + '"'
        if is_const_re.search(line):
            yield lno+1, 'using == None/True/False'
        if lno < 2:
            co = coding_re.search(line)
            if co:
                encoding = co.group(1)
        try:
            line.decode(encoding)
        except UnicodeDecodeError, err:
            yield lno+1, "not decodable: %s\n   Line: %r" % (err, line)
        except LookupError, err:
            yield 0, "unknown encoding: %s" % encoding
            encoding = 'latin1'


@checker('.py', only_pkg=True)
def check_fileheader(fn, lines):
    # line number correction
    c = 1
    if lines[0:1] == ['#!/usr/bin/env python\n']:
        lines = lines[1:]
        c = 2

    llist = []
    docopen = False
    for lno, l in enumerate(lines):
        llist.append(l)
        if lno == 0:
            if l == '# -*- coding: rot13 -*-\n':
                # special-case pony package
                return
            elif l != '# -*- coding: utf-8 -*-\n':
                yield 1, "missing coding declaration"
        elif lno == 1:
            if l != '"""\n' and l != 'r"""\n':
                yield 2, 'missing docstring begin (""")'
            else:
                docopen = True
        elif docopen:
            if l == '"""\n':
                # end of docstring
                if lno <= 4:
                    yield lno+c, "missing module name in docstring"
                break

            if l != "\n" and l[:4] != '    ' and docopen:
                yield lno+c, "missing correct docstring indentation"

            if lno == 2:
                # if not in package, don't check the module name
                modname = fn[:-3].replace('/', '.').replace('.__init__', '')
                while modname:
                    if l.lower()[4:-1] == modname:
                        break
                    modname = '.'.join(modname.split('.')[1:])
                else:
                    yield 3, "wrong module name in docstring heading"
                modnamelen = len(l.strip())
            elif lno == 3:
                if l.strip() != modnamelen * "~":
                    yield 4, "wrong module name underline, should be ~~~...~"

    else:
        yield 0, "missing end and/or start of docstring..."

    # check for copyright and license fields
    license = llist[-2:-1]
    if license != ["    :license: BSD, see LICENSE for details.\n"]:
        yield 0, "no correct license info"

    ci = -3
    copyright = [s.decode('utf-8') for s in llist[ci:ci+1]]
    while copyright and copyright_2_re.match(copyright[0]):
        ci -= 1
        copyright = llist[ci:ci+1]
    if not copyright or not copyright_re.match(copyright[0]):
        yield 0, "no correct copyright info"


@checker('.py', '.html', '.js')
def check_whitespace_and_spelling(fn, lines):
    for lno, line in enumerate(lines):
        if "\t" in line:
            yield lno+1, "OMG TABS!!!1 "
        if line[:-1].rstrip(' \t') != line[:-1]:
            yield lno+1, "trailing whitespace"
        for word in misspellings:
            if word in line and 'ALLOW-MISSPELLING' not in line:
                yield lno+1, '"%s" used' % word


bad_tags = ('<b>', '<i>', '<u>', '<s>', '<strike>'
            '<center>', '<big>', '<small>', '<font')

@checker('.html')
def check_xhtml(fn, lines):
    for lno, line in enumerate(lines):
        for bad_tag in bad_tags:
            if bad_tag in line:
                yield lno+1, "used " + bad_tag


def main(argv):
    try:
        gopts, args = getopt.getopt(argv[1:], "vi:")
    except getopt.GetoptError:
        print "Usage: %s [-v] [-i ignorepath]* [path]" % argv[0]
        return 2
    opts = {}
    for opt, val in gopts:
        if opt == '-i':
            val = abspath(val)
        opts.setdefault(opt, []).append(val)

    if len(args) == 0:
        path = '.'
    elif len(args) == 1:
        path = args[0]
    else:
        print "Usage: %s [-v] [-i ignorepath]* [path]" % argv[0]
        return 2

    verbose = '-v' in opts

    num = 0
    out = cStringIO.StringIO()

    # TODO: replace os.walk run with iteration over output of
    #       `svn list -R`.

    for root, dirs, files in os.walk(path):
        if '.svn' in dirs:
            dirs.remove('.svn')
        if '-i' in opts and abspath(root) in opts['-i']:
            del dirs[:]
            continue
        # XXX: awkward: for the Makefile call: don't check non-package
        #      files for file headers
        in_pocoo_pkg = root.startswith('./pygments')
        for fn in files:

            fn = join(root, fn)
            if fn[:2] == './': fn = fn[2:]

            if '-i' in opts and abspath(fn) in opts['-i']:
                continue

            ext = splitext(fn)[1]
            checkerlist = checkers.get(ext, None)
            if not checkerlist:
                continue

            if verbose:
                print "Checking %s..." % fn

            try:
                f = open(fn, 'r')
                lines = list(f)
            except (IOError, OSError), err:
                print "%s: cannot open: %s" % (fn, err)
                num += 1
                continue

            for checker in checkerlist:
                if not in_pocoo_pkg and checker.only_pkg:
                    continue
                for lno, msg in checker(fn, lines):
                    print >>out, "%s:%d: %s" % (fn, lno, msg)
                    num += 1
    if verbose:
        print
    if num == 0:
        print "No errors found."
    else:
        print out.getvalue().rstrip('\n')
        print "%d error%s found." % (num, num > 1 and "s" or "")
    return int(num > 0)


if __name__ == '__main__':
    sys.exit(main(sys.argv))
