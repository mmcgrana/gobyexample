#!/usr/bin/python
# -*- coding: utf-8 -*-
"""
    Lexing error finder
    ~~~~~~~~~~~~~~~~~~~

    For the source files given on the command line, display
    the text where Error tokens are being generated, along
    with some context.

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from __future__ import print_function

import os
import sys

# always prefer Pygments from source if exists
srcpath = os.path.join(os.path.dirname(__file__), '..')
if os.path.isdir(os.path.join(srcpath, 'pygments')):
    sys.path.insert(0, srcpath)


from pygments.lexer import RegexLexer, ExtendedRegexLexer, LexerContext, \
    ProfilingRegexLexer, ProfilingRegexLexerMeta
from pygments.lexers import get_lexer_by_name, find_lexer_class, \
    find_lexer_class_for_filename
from pygments.token import Error, Text, _TokenType
from pygments.cmdline import _parse_options


class DebuggingRegexLexer(ExtendedRegexLexer):
    """Make the state stack, position and current match instance attributes."""

    def get_tokens_unprocessed(self, text, stack=('root',)):
        """
        Split ``text`` into (tokentype, text) pairs.

        ``stack`` is the inital stack (default: ``['root']``)
        """
        tokendefs = self._tokens
        self.ctx = ctx = LexerContext(text, 0)
        ctx.stack = list(stack)
        statetokens = tokendefs[ctx.stack[-1]]
        while 1:
            for rexmatch, action, new_state in statetokens:
                self.m = m = rexmatch(text, ctx.pos, ctx.end)
                if m:
                    if action is not None:
                        if type(action) is _TokenType:
                            yield ctx.pos, action, m.group()
                            ctx.pos = m.end()
                        else:
                            if not isinstance(self, ExtendedRegexLexer):
                                for item in action(self, m):
                                    yield item
                                ctx.pos = m.end()
                            else:
                                for item in action(self, m, ctx):
                                    yield item
                                if not new_state:
                                    # altered the state stack?
                                    statetokens = tokendefs[ctx.stack[-1]]
                    if new_state is not None:
                        # state transition
                        if isinstance(new_state, tuple):
                            for state in new_state:
                                if state == '#pop':
                                    ctx.stack.pop()
                                elif state == '#push':
                                    ctx.stack.append(ctx.stack[-1])
                                else:
                                    ctx.stack.append(state)
                        elif isinstance(new_state, int):
                            # pop
                            del ctx.stack[new_state:]
                        elif new_state == '#push':
                            ctx.stack.append(ctx.stack[-1])
                        else:
                            assert False, 'wrong state def: %r' % new_state
                        statetokens = tokendefs[ctx.stack[-1]]
                    break
            else:
                try:
                    if ctx.pos >= ctx.end:
                        break
                    if text[ctx.pos] == '\n':
                        # at EOL, reset state to 'root'
                        ctx.stack = ['root']
                        statetokens = tokendefs['root']
                        yield ctx.pos, Text, u'\n'
                        ctx.pos += 1
                        continue
                    yield ctx.pos, Error, text[ctx.pos]
                    ctx.pos += 1
                except IndexError:
                    break


def main(fn, lexer=None, options={}):
    if lexer is not None:
        lxcls = get_lexer_by_name(lexer).__class__
    else:
        lxcls = find_lexer_class_for_filename(os.path.basename(fn))
        if lxcls is None:
            name, rest = fn.split('_', 1)
            lxcls = find_lexer_class(name)
            if lxcls is None:
                raise AssertionError('no lexer found for file %r' % fn)
    debug_lexer = False
    # if profile:
    #     # does not work for e.g. ExtendedRegexLexers
    #     if lxcls.__bases__ == (RegexLexer,):
    #         # yes we can!  (change the metaclass)
    #         lxcls.__class__ = ProfilingRegexLexerMeta
    #         lxcls.__bases__ = (ProfilingRegexLexer,)
    #         lxcls._prof_sort_index = profsort
    # else:
    #     if lxcls.__bases__ == (RegexLexer,):
    #         lxcls.__bases__ = (DebuggingRegexLexer,)
    #         debug_lexer = True
    #     elif lxcls.__bases__ == (DebuggingRegexLexer,):
    #         # already debugged before
    #         debug_lexer = True
    #     else:
    #         # HACK: ExtendedRegexLexer subclasses will only partially work here.
    #         lxcls.__bases__ = (DebuggingRegexLexer,)
    #         debug_lexer = True

    lx = lxcls(**options)
    lno = 1
    if fn == '-':
        text = sys.stdin.read()
    else:
        with open(fn, 'rb') as fp:
            text = fp.read().decode('utf-8')
    text = text.strip('\n') + '\n'
    tokens = []
    states = []

    def show_token(tok, state):
        reprs = list(map(repr, tok))
        print('   ' + reprs[1] + ' ' + ' ' * (29-len(reprs[1])) + reprs[0], end=' ')
        if debug_lexer:
            print(' ' + ' ' * (29-len(reprs[0])) + ' : '.join(state) if state else '', end=' ')
        print()

    for type, val in lx.get_tokens(text):
        lno += val.count('\n')
        if type == Error and not ignerror:
            print('Error parsing', fn, 'on line', lno)
            if not showall:
                print('Previous tokens' + (debug_lexer and ' and states' or '') + ':')
                for i in range(max(len(tokens) - num, 0), len(tokens)):
                    if debug_lexer:
                        show_token(tokens[i], states[i])
                    else:
                        show_token(tokens[i], None)
            print('Error token:')
            l = len(repr(val))
            print('   ' + repr(val), end=' ')
            if debug_lexer and hasattr(lx, 'ctx'):
                print(' ' * (60-l) + ' : '.join(lx.ctx.stack), end=' ')
            print()
            print()
            return 1
        tokens.append((type, val))
        if debug_lexer:
            if hasattr(lx, 'ctx'):
                states.append(lx.ctx.stack[:])
            else:
                states.append(None)
        if showall:
            show_token((type, val), states[-1] if debug_lexer else None)
    return 0


def print_help():
    print('''\
Pygments development helper to quickly debug lexers.

    scripts/debug_lexer.py [options] file ...

Give one or more filenames to lex them and display possible error tokens
and/or profiling info.  Files are assumed to be encoded in UTF-8.

Selecting lexer and options:

    -l NAME         use lexer named NAME (default is to guess from
                    the given filenames)
    -O OPTIONSTR    use lexer options parsed from OPTIONSTR

Debugging lexing errors:

    -n N            show the last N tokens on error
    -a              always show all lexed tokens (default is only
                    to show them when an error occurs)
    -e              do not stop on error tokens

Profiling:

    -p              use the ProfilingRegexLexer to profile regexes
                    instead of the debugging lexer
    -s N            sort profiling output by column N (default is
                    column 4, the time per call)
''')

num = 10
showall = False
ignerror = False
lexer = None
options = {}
profile = False
profsort = 4

if __name__ == '__main__':
    import getopt
    opts, args = getopt.getopt(sys.argv[1:], 'n:l:aepO:s:h')
    for opt, val in opts:
        if opt == '-n':
            num = int(val)
        elif opt == '-a':
            showall = True
        elif opt == '-e':
            ignerror = True
        elif opt == '-l':
            lexer = val
        elif opt == '-p':
            profile = True
        elif opt == '-s':
            profsort = int(val)
        elif opt == '-O':
            options = _parse_options([val])
        elif opt == '-h':
            print_help()
            sys.exit(0)
    ret = 0
    if not args:
        print_help()
    for f in args:
        ret += main(f, lexer, options)
    sys.exit(bool(ret))
