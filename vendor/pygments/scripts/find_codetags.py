#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    Codetags finder
    ~~~~~~~~~~~~~~~

    Find code tags in specified files and/or directories
    and create a report in HTML format.

    :copyright: Copyright 2006-2013 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import sys, os, re
import getopt
from os.path import join, abspath, isdir, isfile


TAGS = set(('XXX', 'TODO', 'FIXME', 'HACK'))

tag_re = re.compile(
    r'(?P<tag>\b' + r'\b|\b'.join(TAGS) + r'\b)\s*'
    r'(?: \( (?P<who> .*? ) \) )?'
    r'\s*:?\s* (?P<what> .*? ) \s* $',
    re.X)

binary_re = re.compile('[\x00-\x06\x0E-\x1F]')


def escape_html(text):
    return text.replace('&', '&amp;'). \
                replace('<', '&lt;').  \
                replace('>', '&gt;').  \
                replace('"', '&quot;')

def process_file(store, filename):
    try:
        f = open(filename, 'r')
    except (IOError, OSError):
        return False
    llmatch = 0
    try:
        for lno, line in enumerate(f):
            # just some random heuristics to filter out binary files
            if lno < 100 and binary_re.search(line):
                return False
            m = tag_re.search(line)
            if m:
                store.setdefault(filename, []).append({
                    'lno':  lno+1,
                    'tag':  m.group('tag'),
                    'who':  m.group('who') or '',
                    'what': escape_html(m.group('what')),
                })
                # 'what' cannot start at column 0
                llmatch = m.start('what')
            elif llmatch:
                # continuation lines
                # XXX: this is Python centric, doesn't work for
                #      JavaScript, for example.
                if line[:llmatch].replace('#', '').isspace():
                    cont = line[llmatch:].strip()
                    if cont:
                        store[filename][-1]['what'] += ' ' + escape_html(cont)
                        continue
                llmatch = 0
        return True
    finally:
        f.close()


def main():
    try:
        gopts, args = getopt.getopt(sys.argv[1:], "vo:i:")
    except getopt.GetoptError:
        print ("Usage: %s [-v] [-i ignoredir]* [-o reportfile.html] "
               "path ..." % sys.argv[0])
        return 2
    opts = {}
    for opt, val in gopts:
        if opt == '-i':
            val = abspath(val)
        opts.setdefault(opt, []).append(val)

    if not args:
        args = ['.']

    if '-o' in opts:
        output = abspath(opts['-o'][-1])
    else:
        output = abspath('tags.html')

    verbose = '-v' in opts

    store = {}
    gnum = 0
    num = 0

    for path in args:
        print "Searching for code tags in %s, please wait." % path

        if isfile(path):
            gnum += 1
            if process_file(store, path):
                if verbose:
                    print path + ": found %d tags" % \
                        (path in store and len(store[path]) or 0)
                num += 1
            else:
                if verbose:
                    print path + ": binary or not readable"
            continue
        elif not isdir(path):
            continue

        for root, dirs, files in os.walk(path):
            if '-i' in opts and abspath(root) in opts['-i']:
                del dirs[:]
                continue
            if '.svn' in dirs:
                dirs.remove('.svn')
            for fn in files:
                gnum += 1
                if gnum % 50 == 0 and not verbose:
                    sys.stdout.write('.')
                    sys.stdout.flush()

                fn = join(root, fn)

                if fn.endswith('.pyc') or fn.endswith('.pyo'):
                    continue
                elif '-i' in opts and abspath(fn) in opts['-i']:
                    continue
                elif abspath(fn) == output:
                    continue

                if fn[:2] == './': fn = fn[2:]
                if process_file(store, fn):
                    if verbose:
                        print fn + ": found %d tags" % \
                            (fn in store and len(store[fn]) or 0)
                    num += 1
                else:
                    if verbose:
                        print fn + ": binary or not readable"
        print

    print "Processed %d of %d files. Found %d tags in %d files." % (
        num, gnum, sum(len(fitem) for fitem in store.itervalues()), len(store))

    if not store:
        return 0

    HTML = '''\
<html>
<head>
<title>Code tags report</title>
<style type="text/css">
body { font-family: Trebuchet MS,Verdana,sans-serif;
       width: 80%%; margin-left: auto; margin-right: auto; }
table { width: 100%%; border-spacing: 0;
        border: 1px solid #CCC; }
th { font-weight: bold; background-color: #DDD }
td { padding: 2px 5px 2px 5px;
     vertical-align: top; }
.tr0 { background-color: #EEEEEE; }
.tr1 { background-color: #F6F6F6; }
.tag { text-align: center; font-weight: bold; }
.tr0 .tag { background-color: #FFEEEE; }
.tr1 .tag { background-color: #FFDDDD; }
.head { padding-top: 10px; font-size: 100%%; font-weight: bold }
.XXX { color: #500; }
.FIXME { color: red; }
.TODO { color: #880; }
</style>
</head>
<body>
<h1>Code tags report for %s</h1>
<table>
<tr><th>Line</th><th>Tag</th><th>Who</th><th>Description</th></tr>
%s
</table>
</body>
</html>
'''

    TABLE = '\n<tr><td class="head" colspan="4">File: %s</td>\n'

    TR = ('<tr class="tr%d"><td class="lno">%%(lno)d</td>'
          '<td class="tag %%(tag)s">%%(tag)s</td>'
          '<td class="who">%%(who)s</td><td class="what">%%(what)s</td></tr>')

    f = file(output, 'w')
    table = '\n'.join(TABLE % fname +
                      '\n'.join(TR % (no % 2,) % entry
                                for no, entry in enumerate(store[fname]))
                      for fname in sorted(store))
    f.write(HTML % (', '.join(map(abspath, args)), table))
    f.close()

    print "Report written to %s." % output
    return 0

if __name__ == '__main__':
    sys.exit(main())
