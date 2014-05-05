#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    Generate Pygments Documentation
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Generates a bunch of html files containing the documentation.

    :copyright: Copyright 2006-2013 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import os
import sys
from datetime import datetime
from cgi import escape

from docutils import nodes
from docutils.parsers.rst import directives
from docutils.core import publish_parts
from docutils.writers import html4css1

from jinja2 import Template

# try to use the right Pygments to build the docs
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from pygments import highlight, __version__
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter


LEXERDOC = '''
`%s`
%s
    :Short names: %s
    :Filename patterns: %s
    :Mimetypes: %s

'''

def generate_lexer_docs():
    from pygments.lexers import LEXERS

    out = []

    modules = {}
    moduledocstrings = {}
    for classname, data in sorted(LEXERS.iteritems(), key=lambda x: x[0]):
        module = data[0]
        mod = __import__(module, None, None, [classname])
        cls = getattr(mod, classname)
        if not cls.__doc__:
            print "Warning: %s does not have a docstring." % classname
        modules.setdefault(module, []).append((
            classname,
            cls.__doc__,
            ', '.join(data[2]) or 'None',
            ', '.join(data[3]).replace('*', '\\*').replace('_', '\\') or 'None',
            ', '.join(data[4]) or 'None'))
        if module not in moduledocstrings:
            moduledocstrings[module] = mod.__doc__

    for module, lexers in sorted(modules.iteritems(), key=lambda x: x[0]):
        heading = moduledocstrings[module].splitlines()[4].strip().rstrip('.')
        out.append('\n' + heading + '\n' + '-'*len(heading) + '\n')
        for data in lexers:
            out.append(LEXERDOC % data)
    return ''.join(out).decode('utf-8')

def generate_formatter_docs():
    from pygments.formatters import FORMATTERS

    out = []
    for cls, data in sorted(FORMATTERS.iteritems(),
                            key=lambda x: x[0].__name__):
        heading = cls.__name__
        out.append('`' + heading + '`\n' + '-'*(2+len(heading)) + '\n')
        out.append(cls.__doc__)
        out.append('''
    :Short names: %s
    :Filename patterns: %s


''' % (', '.join(data[1]) or 'None', ', '.join(data[2]).replace('*', '\\*') or 'None'))
    return ''.join(out).decode('utf-8')

def generate_filter_docs():
    from pygments.filters import FILTERS

    out = []
    for name, cls in FILTERS.iteritems():
        out.append('''
`%s`
%s
    :Name: %s
''' % (cls.__name__, cls.__doc__, name))
    return ''.join(out).decode('utf-8')

def generate_changelog():
    fn = os.path.abspath(os.path.join(os.path.dirname(__file__), '..',
                         'CHANGES'))
    f = file(fn)
    result = []
    in_header = False
    header = True
    for line in f:
        if header:
            if not in_header and line.strip():
                in_header = True
            elif in_header and not line.strip():
                header = False
        else:
            result.append(line.rstrip())
    f.close()
    return '\n'.join(result).decode('utf-8')

def generate_authors():
    fn = os.path.abspath(os.path.join(os.path.dirname(__file__), '..',
                         'AUTHORS'))
    f = file(fn)
    r = f.read().rstrip().decode('utf-8')
    f.close()
    return r

LEXERDOCS = generate_lexer_docs()
FORMATTERDOCS = generate_formatter_docs()
FILTERDOCS = generate_filter_docs()
CHANGELOG = generate_changelog()
AUTHORS = generate_authors()


PYGMENTS_FORMATTER = HtmlFormatter(style='pastie', cssclass='syntax')

USAGE = '''\
Usage: %s <mode> <destination> [<source.txt> ...]

Generate either python or html files out of the documentation.

Mode can either be python or html.\
''' % sys.argv[0]

TEMPLATE = '''\
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
  <title>{{ title }} &mdash; Pygments</title>
  <meta http-equiv="content-type" content="text/html; charset=utf-8">
  <style type="text/css">
    {{ style }}
  </style>
</head>
<body>
  <div id="content">
    <h1 class="heading">Pygments</h1>
    <h2 class="subheading">{{ title }}</h2>
    {% if file_id != "index" %}
      <a id="backlink" href="index.html">&laquo; Back To Index</a>
    {% endif %}
    {% if toc %}
      <div class="toc">
        <h2>Contents</h2>
        <ul class="contents">
        {% for key, value in toc %}
          <li><a href="{{ key }}">{{ value }}</a></li>
        {% endfor %}
        </ul>
      </div>
    {% endif %}
    {{ body }}
  </div>
</body>
<!-- generated on: {{ generation_date }}
     file id: {{ file_id }} -->
</html>\
'''

STYLESHEET = '''\
body {
    background-color: #f2f2f2;
    margin: 0;
    padding: 0;
    font-family: 'Georgia', serif;
    color: #111;
}

#content {
    background-color: white;
    padding: 20px;
    margin: 20px auto 20px auto;
    max-width: 800px;
    border: 4px solid #ddd;
}

h1 {
    font-weight: normal;
    font-size: 40px;
    color: #09839A;
}

h2 {
    font-weight: normal;
    font-size: 30px;
    color: #C73F00;
}

h1.heading {
    margin: 0 0 30px 0;
}

h2.subheading {
    margin: -30px 0 0 45px;
}

h3 {
    margin-top: 30px;
}

table.docutils {
    border-collapse: collapse;
    border: 2px solid #aaa;
    margin: 0.5em 1.5em 0.5em 1.5em;
}

table.docutils td {
    padding: 2px;
    border: 1px solid #ddd;
}

p, li, dd, dt, blockquote {
    font-size: 15px;
    color: #333;
}

p {
    line-height: 150%;
    margin-bottom: 0;
    margin-top: 10px;
}

hr {
    border-top: 1px solid #ccc;
    border-bottom: 0;
    border-right: 0;
    border-left: 0;
    margin-bottom: 10px;
    margin-top: 20px;
}

dl {
    margin-left: 10px;
}

li, dt {
    margin-top: 5px;
}

dt {
    font-weight: bold;
}

th {
    text-align: left;
}

a {
    color: #990000;
}

a:hover {
    color: #c73f00;
}

pre {
    background-color: #f9f9f9;
    border-top: 1px solid #ccc;
    border-bottom: 1px solid #ccc;
    padding: 5px;
    font-size: 13px;
    font-family: Bitstream Vera Sans Mono,monospace;
}

tt {
    font-size: 13px;
    font-family: Bitstream Vera Sans Mono,monospace;
    color: black;
    padding: 1px 2px 1px 2px;
    background-color: #f0f0f0;
}

cite {
    /* abusing <cite>, it's generated by ReST for `x` */
    font-size: 13px;
    font-family: Bitstream Vera Sans Mono,monospace;
    font-weight: bold;
    font-style: normal;
}

#backlink {
    float: right;
    font-size: 11px;
    color: #888;
}

div.toc {
    margin: 0 0 10px 0;
}

div.toc h2 {
    font-size: 20px;
}
''' #'


def pygments_directive(name, arguments, options, content, lineno,
                      content_offset, block_text, state, state_machine):
    try:
        lexer = get_lexer_by_name(arguments[0])
    except ValueError:
        # no lexer found
        lexer = get_lexer_by_name('text')
    parsed = highlight(u'\n'.join(content), lexer, PYGMENTS_FORMATTER)
    return [nodes.raw('', parsed, format="html")]
pygments_directive.arguments = (1, 0, 1)
pygments_directive.content = 1
directives.register_directive('sourcecode', pygments_directive)


def create_translator(link_style):
    class Translator(html4css1.HTMLTranslator):
        def visit_reference(self, node):
            refuri = node.get('refuri')
            if refuri is not None and '/' not in refuri and refuri.endswith('.txt'):
                node['refuri'] = link_style(refuri[:-4])
            html4css1.HTMLTranslator.visit_reference(self, node)
    return Translator


class DocumentationWriter(html4css1.Writer):

    def __init__(self, link_style):
        html4css1.Writer.__init__(self)
        self.translator_class = create_translator(link_style)

    def translate(self):
        html4css1.Writer.translate(self)
        # generate table of contents
        contents = self.build_contents(self.document)
        contents_doc = self.document.copy()
        contents_doc.children = contents
        contents_visitor = self.translator_class(contents_doc)
        contents_doc.walkabout(contents_visitor)
        self.parts['toc'] = self._generated_toc

    def build_contents(self, node, level=0):
        sections = []
        i = len(node) - 1
        while i >= 0 and isinstance(node[i], nodes.section):
            sections.append(node[i])
            i -= 1
        sections.reverse()
        toc = []
        for section in sections:
            try:
                reference = nodes.reference('', '', refid=section['ids'][0], *section[0])
            except IndexError:
                continue
            ref_id = reference['refid']
            text = escape(reference.astext())
            toc.append((ref_id, text))

        self._generated_toc = [('#%s' % href, caption) for href, caption in toc]
        # no further processing
        return []


def generate_documentation(data, link_style):
    writer = DocumentationWriter(link_style)
    data = data.replace('[builtin_lexer_docs]', LEXERDOCS).\
                replace('[builtin_formatter_docs]', FORMATTERDOCS).\
                replace('[builtin_filter_docs]', FILTERDOCS).\
                replace('[changelog]', CHANGELOG).\
                replace('[authors]', AUTHORS)
    parts = publish_parts(
        data,
        writer=writer,
        settings_overrides={
            'initial_header_level': 3,
            'field_name_limit': 50,
        }
    )
    return {
        'title':        parts['title'],
        'body':         parts['body'],
        'toc':          parts['toc']
    }


def handle_python(filename, fp, dst):
    now = datetime.now()
    title = os.path.basename(filename)[:-4]
    content = fp.read()
    def urlize(href):
        # create links for the pygments webpage
        if href == 'index.txt':
            return '/docs/'
        else:
            return '/docs/%s/' % href
    parts = generate_documentation(content, urlize)
    result = file(os.path.join(dst, title + '.py'), 'w')
    result.write('# -*- coding: utf-8 -*-\n')
    result.write('"""\n    Pygments Documentation - %s\n' % title)
    result.write('    %s\n\n' % ('~' * (24 + len(title))))
    result.write('    Generated on: %s\n"""\n\n' % now)
    result.write('import datetime\n')
    result.write('DATE = %r\n' % now)
    result.write('TITLE = %r\n' % parts['title'])
    result.write('TOC = %r\n' % parts['toc'])
    result.write('BODY = %r\n' % parts['body'])
    result.close()


def handle_html(filename, fp, dst):
    now = datetime.now()
    title = os.path.basename(filename)[:-4]
    content = fp.read().decode('utf-8')
    c = generate_documentation(content, (lambda x: './%s.html' % x))
    result = file(os.path.join(dst, title + '.html'), 'w')
    c['style'] = STYLESHEET + PYGMENTS_FORMATTER.get_style_defs('.syntax')
    c['generation_date'] = now
    c['file_id'] = title
    t = Template(TEMPLATE)
    result.write(t.render(c).encode('utf-8'))
    result.close()


def run(handle_file, dst, sources=()):
    path = os.path.abspath(os.path.join(os.path.dirname(__file__), 'src'))
    if not sources:
        sources = [os.path.join(path, fn) for fn in os.listdir(path)]
    if not os.path.isdir(dst):
        os.makedirs(dst)
    print 'Making docs for Pygments %s in %s' % (__version__, dst)
    for fn in sources:
        if not os.path.isfile(fn):
            continue
        print 'Processing %s' % fn
        f = open(fn)
        try:
            handle_file(fn, f, dst)
        finally:
            f.close()


def main(mode, dst='build/', *sources):
    try:
        handler = {
            'html':         handle_html,
            'python':       handle_python
        }[mode]
    except KeyError:
        print 'Error: unknown mode "%s"' % mode
        sys.exit(1)
    run(handler, os.path.realpath(dst), sources)


if __name__ == '__main__':
    if len(sys.argv) == 1:
        print USAGE
    else:
        main(*sys.argv[1:])
