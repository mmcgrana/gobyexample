# -*- coding: utf-8 -*-
"""
    The Pygments MoinMoin Parser
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    This is a MoinMoin parser plugin that renders source code to HTML via
    Pygments; you need Pygments 0.7 or newer for this parser to work.

    To use it, set the options below to match your setup and put this file in
    the data/plugin/parser subdirectory of your Moin instance, and give it the
    name that the parser directive should have. For example, if you name the
    file ``code.py``, you can get a highlighted Python code sample with this
    Wiki markup::

        {{{
        #!code python
        [...]
        }}}

    Additionally, if you set ATTACHMENTS below to True, Pygments will also be
    called for all attachments for whose filenames there is no other parser
    registered.

    You are responsible for including CSS rules that will map the Pygments CSS
    classes to colors. You can output a stylesheet file with `pygmentize`, put
    it into the `htdocs` directory of your Moin instance and then include it in
    the `stylesheets` configuration option in the Moin config, e.g.::

        stylesheets = [('screen', '/htdocs/pygments.css')]

    If you do not want to do that and are willing to accept larger HTML
    output, you can set the INLINESTYLES option below to True.

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

# Options
# ~~~~~~~

# Set to True if you want to highlight attachments, in addition to
# {{{ }}} blocks.
ATTACHMENTS = True

# Set to True if you want inline CSS styles instead of classes
INLINESTYLES = False


import sys

from pygments import highlight
from pygments.lexers import get_lexer_by_name, get_lexer_for_filename, TextLexer
from pygments.formatters import HtmlFormatter
from pygments.util import ClassNotFound


# wrap lines in <span>s so that the Moin-generated line numbers work
class MoinHtmlFormatter(HtmlFormatter):
    def wrap(self, source, outfile):
        for line in source:
            yield 1, '<span class="line">' + line[1] + '</span>'

htmlformatter = MoinHtmlFormatter(noclasses=INLINESTYLES)
textlexer = TextLexer()
codeid = [0]


class Parser:
    """
    MoinMoin Pygments parser.
    """
    if ATTACHMENTS:
        extensions = '*'
    else:
        extensions = []

    Dependencies = []

    def __init__(self, raw, request, **kw):
        self.raw = raw
        self.req = request
        if "format_args" in kw:
            # called from a {{{ }}} block
            try:
                self.lexer = get_lexer_by_name(kw['format_args'].strip())
            except ClassNotFound:
                self.lexer = textlexer
            return
        if "filename" in kw:
            # called for an attachment
            filename = kw['filename']
        else:
            # called for an attachment by an older moin
            # HACK: find out the filename by peeking into the execution
            #       frame which might not always work
            try:
                frame = sys._getframe(1)
                filename = frame.f_locals['filename']
            except:
                filename = 'x.txt'
        try:
            self.lexer = get_lexer_for_filename(filename)
        except ClassNotFound:
            self.lexer = textlexer

    def format(self, formatter):
        codeid[0] += 1
        id = "pygments_%s" % codeid[0]
        w = self.req.write
        w(formatter.code_area(1, id, start=1, step=1))
        w(formatter.rawHTML(highlight(self.raw, self.lexer, htmlformatter)))
        w(formatter.code_area(0, id))
