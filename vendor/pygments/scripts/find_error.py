#!/usr/bin/python
# -*- coding: utf-8 -*-
"""
    Lexing error finder
    ~~~~~~~~~~~~~~~~~~~

    For the source files given on the command line, display
    the text where Error tokens are being generated, along
    with some context.

    :copyright: Copyright 2006-2013 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import sys, os

# always prefer Pygments from source if exists
srcpath = os.path.join(os.path.dirname(__file__), '..')
if os.path.isdir(os.path.join(srcpath, 'pygments')):
    sys.path.insert(0, srcpath)


from pygments.lexer import RegexLexer
from pygments.lexers import get_lexer_for_filename, get_lexer_by_name
from pygments.token import Error, Text, _TokenType
from pygments.cmdline import _parse_options


class DebuggingRegexLexer(RegexLexer):
    """Make the state stack, position and current match instance attributes."""

    def get_tokens_unprocessed(self, text, stack=('root',)):
        """
        Split ``text`` into (tokentype, text) pairs.

        ``stack`` is the inital stack (default: ``['root']``)
        """
        self.pos = 0
        tokendefs = self._tokens
        self.statestack = list(stack)
        statetokens = tokendefs[self.statestack[-1]]
        while 1:
            for rexmatch, action, new_state in statetokens:
                self.m = m = rexmatch(text, self.pos)
                if m:
                    if type(action) is _TokenType:
                        yield self.pos, action, m.group()
                    else:
                        for item in action(self, m):
                            yield item
                    self.pos = m.end()
                    if new_state is not None:
                        # state transition
                        if isinstance(new_state, tuple):
                            for state in new_state:
                                if state == '#pop':
                                    self.statestack.pop()
                                elif state == '#push':
                                    self.statestack.append(self.statestack[-1])
                                else:
                                    self.statestack.append(state)
                        elif isinstance(new_state, int):
                            # pop
                            del self.statestack[new_state:]
                        elif new_state == '#push':
                            self.statestack.append(self.statestack[-1])
                        else:
                            assert False, 'wrong state def: %r' % new_state
                        statetokens = tokendefs[self.statestack[-1]]
                    break
            else:
                try:
                    if text[self.pos] == '\n':
                        # at EOL, reset state to 'root'
                        self.pos += 1
                        self.statestack = ['root']
                        statetokens = tokendefs['root']
                        yield self.pos, Text, u'\n'
                        continue
                    yield self.pos, Error, text[self.pos]
                    self.pos += 1
                except IndexError:
                    break


def main(fn, lexer=None, options={}):
    if lexer is not None:
        lx = get_lexer_by_name(lexer)
    else:
        try:
            lx = get_lexer_for_filename(os.path.basename(fn), **options)
        except ValueError:
            try:
                name, rest = fn.split('_', 1)
                lx = get_lexer_by_name(name, **options)
            except ValueError:
                raise AssertionError('no lexer found for file %r' % fn)
    debug_lexer = False
    # does not work for e.g. ExtendedRegexLexers
    if lx.__class__.__bases__ == (RegexLexer,):
        lx.__class__.__bases__ = (DebuggingRegexLexer,)
        debug_lexer = True
    elif lx.__class__.__bases__ == (DebuggingRegexLexer,):
        # already debugged before
        debug_lexer = True
    lno = 1
    text = file(fn, 'U').read()
    text = text.strip('\n') + '\n'
    tokens = []
    states = []

    def show_token(tok, state):
        reprs = map(repr, tok)
        print '   ' + reprs[1] + ' ' + ' ' * (29-len(reprs[1])) + reprs[0],
        if debug_lexer:
            print ' ' + ' ' * (29-len(reprs[0])) + repr(state),
        print

    for type, val in lx.get_tokens(text):
        lno += val.count('\n')
        if type == Error:
            print 'Error parsing', fn, 'on line', lno
            print 'Previous tokens' + (debug_lexer and ' and states' or '') + ':'
            if showall:
                for tok, state in map(None, tokens, states):
                    show_token(tok, state)
            else:
                for i in range(max(len(tokens) - num, 0), len(tokens)):
                    show_token(tokens[i], states[i])
            print 'Error token:'
            l = len(repr(val))
            print '   ' + repr(val),
            if debug_lexer and hasattr(lx, 'statestack'):
                print ' ' * (60-l) + repr(lx.statestack),
            print
            print
            return 1
        tokens.append((type, val))
        if debug_lexer:
            if hasattr(lx, 'statestack'):
                states.append(lx.statestack[:])
            else:
                states.append(None)
    if showall:
        for tok, state in map(None, tokens, states):
            show_token(tok, state)
    return 0


num = 10
showall = False
lexer = None
options = {}

if __name__ == '__main__':
    import getopt
    opts, args = getopt.getopt(sys.argv[1:], 'n:l:aO:')
    for opt, val in opts:
        if opt == '-n':
            num = int(val)
        elif opt == '-a':
            showall = True
        elif opt == '-l':
            lexer = val
        elif opt == '-O':
            options = _parse_options([val])
    ret = 0
    for f in args:
        ret += main(f, lexer, options)
    sys.exit(bool(ret))
