import re
from pprint import pprint

r_line = re.compile(r"^(syn keyword vimCommand contained|syn keyword vimOption "
                    r"contained|syn keyword vimAutoEvent contained)\s+(.*)")
r_item = re.compile(r"(\w+)(?:\[(\w+)\])?")

def getkw(input, output):
    out = file(output, 'w')

    output_info = {'command': [], 'option': [], 'auto': []}
    for line in file(input):
        m = r_line.match(line)
        if m:
            # Decide which output gets mapped to d
            if 'vimCommand' in m.group(1):
                d = output_info['command']
            elif 'AutoEvent' in m.group(1):
                d = output_info['auto']
            else:
                d = output_info['option']

            # Extract all the shortened versions
            for i in r_item.finditer(m.group(2)):
                d.append('(%r,%r)' %
                         (i.group(1), "%s%s" % (i.group(1), i.group(2) or '')))

    output_info['option'].append("('nnoremap','nnoremap')")
    output_info['option'].append("('inoremap','inoremap')")
    output_info['option'].append("('vnoremap','vnoremap')")

    for a, b in output_info.items():
        b.sort()
        print >>out, '%s=[%s]' % (a, ','.join(b))

def is_keyword(w, keywords):
    for i in range(len(w), 0, -1):
        if w[:i] in keywords:
            return signals[w[:i]][:len(w)] == w
    return False

if __name__ == "__main__":
    getkw("/usr/share/vim/vim73/syntax/vim.vim", "temp.py")
