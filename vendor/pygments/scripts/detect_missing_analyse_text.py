import sys

from pygments.lexers import get_all_lexers, find_lexer_class
from pygments.lexer import Lexer

def main():
    uses = {}

    for name, aliases, filenames, mimetypes in get_all_lexers():
        cls = find_lexer_class(name)
        if not cls.aliases:
            print cls, "has no aliases"
        for f in filenames:
            if f not in uses:
                uses[f] = []
            uses[f].append(cls)

    ret = 0
    for k, v in uses.iteritems():
        if len(v) > 1:
            #print "Multiple for", k, v
            for i in v:
                if i.analyse_text is None:
                    print i, "has a None analyse_text"
                    ret |= 1
                elif Lexer.analyse_text.__doc__ == i.analyse_text.__doc__:
                    print i, "needs analyse_text, multiple lexers for", k
                    ret |= 2
    return ret

if __name__ == '__main__':
    sys.exit(main())
