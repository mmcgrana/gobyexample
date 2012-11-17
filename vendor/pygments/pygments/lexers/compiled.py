# -*- coding: utf-8 -*-
"""
    pygments.lexers.compiled
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for compiled languages.

    :copyright: Copyright 2006-2012 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re
from string import Template

from pygments.lexer import Lexer, RegexLexer, include, bygroups, using, \
     this, combined
from pygments.util import get_bool_opt, get_list_opt
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
     Number, Punctuation, Error, Literal
from pygments.scanner import Scanner

# backwards compatibility
from pygments.lexers.functional import OcamlLexer
from pygments.lexers.jvm import JavaLexer, ScalaLexer

__all__ = ['CLexer', 'CppLexer', 'DLexer', 'DelphiLexer', 'ECLexer',
           'DylanLexer', 'ObjectiveCLexer', 'FortranLexer', 'GLShaderLexer',
           'PrologLexer', 'CythonLexer', 'ValaLexer', 'OocLexer', 'GoLexer',
           'FelixLexer', 'AdaLexer', 'Modula2Lexer', 'BlitzMaxLexer',
           'NimrodLexer', 'FantomLexer', 'RustLexer', 'CUDALexer']


class CLexer(RegexLexer):
    """
    For C source code with preprocessor directives.
    """
    name = 'C'
    aliases = ['c']
    filenames = ['*.c', '*.h', '*.idc']
    mimetypes = ['text/x-chdr', 'text/x-csrc']

    #: optional Comment or Whitespace
    _ws = r'(?:\s|//.*?\n|/[*].*?[*]/)+'
    #: only one /* */ style comment
    _ws1 = r':\s*/[*].*?[*]/\s*'

    tokens = {
        'whitespace': [
            # preprocessor directives: without whitespace
            ('^#if\s+0', Comment.Preproc, 'if0'),
            ('^#', Comment.Preproc, 'macro'),
            # or with whitespace
            ('^(' + _ws1 + r')(#if\s+0)',
             bygroups(using(this), Comment.Preproc), 'if0'),
            ('^(' + _ws1 + ')(#)',
             bygroups(using(this), Comment.Preproc), 'macro'),
            (r'^(\s*)([a-zA-Z_][a-zA-Z0-9_]*:(?!:))',
             bygroups(Text, Name.Label)),
            (r'\n', Text),
            (r'\s+', Text),
            (r'\\\n', Text), # line continuation
            (r'//(\n|(.|\n)*?[^\\]\n)', Comment.Single),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment.Multiline),
        ],
        'statements': [
            (r'L?"', String, 'string'),
            (r"L?'(\\.|\\[0-7]{1,3}|\\x[a-fA-F0-9]{1,2}|[^\\\'\n])'", String.Char),
            (r'(\d+\.\d*|\.\d+|\d+)[eE][+-]?\d+[LlUu]*', Number.Float),
            (r'(\d+\.\d*|\.\d+|\d+[fF])[fF]?', Number.Float),
            (r'0x[0-9a-fA-F]+[LlUu]*', Number.Hex),
            (r'0[0-7]+[LlUu]*', Number.Oct),
            (r'\d+[LlUu]*', Number.Integer),
            (r'\*/', Error),
            (r'[~!%^&*+=|?:<>/-]', Operator),
            (r'[()\[\],.]', Punctuation),
            (r'\b(case)(.+?)(:)', bygroups(Keyword, using(this), Text)),
            (r'(auto|break|case|const|continue|default|do|else|enum|extern|'
             r'for|goto|if|register|restricted|return|sizeof|static|struct|'
             r'switch|typedef|union|volatile|virtual|while)\b', Keyword),
            (r'(int|long|float|short|double|char|unsigned|signed|void)\b',
             Keyword.Type),
            (r'(_{0,2}inline|naked|restrict|thread|typename)\b', Keyword.Reserved),
            (r'__(asm|int8|based|except|int16|stdcall|cdecl|fastcall|int32|'
             r'declspec|finally|int64|try|leave)\b', Keyword.Reserved),
            (r'(true|false|NULL)\b', Name.Builtin),
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name),
        ],
        'root': [
            include('whitespace'),
            # functions
            (r'((?:[a-zA-Z0-9_*\s])+?(?:\s|[*]))'    # return arguments
             r'([a-zA-Z_][a-zA-Z0-9_]*)'             # method name
             r'(\s*\([^;]*?\))'                      # signature
             r'(' + _ws + r')?({)',
             bygroups(using(this), Name.Function, using(this), using(this),
                      Punctuation),
             'function'),
            # function declarations
            (r'((?:[a-zA-Z0-9_*\s])+?(?:\s|[*]))'    # return arguments
             r'([a-zA-Z_][a-zA-Z0-9_]*)'             # method name
             r'(\s*\([^;]*?\))'                      # signature
             r'(' + _ws + r')?(;)',
             bygroups(using(this), Name.Function, using(this), using(this),
                      Punctuation)),
            ('', Text, 'statement'),
        ],
        'statement' : [
            include('whitespace'),
            include('statements'),
            ('[{}]', Punctuation),
            (';', Punctuation, '#pop'),
        ],
        'function': [
            include('whitespace'),
            include('statements'),
            (';', Punctuation),
            ('{', Punctuation, '#push'),
            ('}', Punctuation, '#pop'),
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'\\([\\abfnrtv"\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})', String.Escape),
            (r'[^\\"\n]+', String), # all other characters
            (r'\\\n', String), # line continuation
            (r'\\', String), # stray backslash
        ],
        'macro': [
            (r'[^/\n]+', Comment.Preproc),
            (r'/[*](.|\n)*?[*]/', Comment.Multiline),
            (r'//.*?\n', Comment.Single, '#pop'),
            (r'/', Comment.Preproc),
            (r'(?<=\\)\n', Comment.Preproc),
            (r'\n', Comment.Preproc, '#pop'),
        ],
        'if0': [
            (r'^\s*#if.*?(?<!\\)\n', Comment.Preproc, '#push'),
            (r'^\s*#el(?:se|if).*\n', Comment.Preproc, '#pop'),
            (r'^\s*#endif.*?(?<!\\)\n', Comment.Preproc, '#pop'),
            (r'.*?\n', Comment),
        ]
    }

    stdlib_types = ['size_t', 'ssize_t', 'off_t', 'wchar_t', 'ptrdiff_t',
            'sig_atomic_t', 'fpos_t', 'clock_t', 'time_t', 'va_list',
            'jmp_buf', 'FILE', 'DIR', 'div_t', 'ldiv_t', 'mbstate_t',
            'wctrans_t', 'wint_t', 'wctype_t']
    c99_types = ['_Bool', '_Complex', 'int8_t', 'int16_t', 'int32_t', 'int64_t',
            'uint8_t', 'uint16_t', 'uint32_t', 'uint64_t', 'int_least8_t',
            'int_least16_t', 'int_least32_t', 'int_least64_t',
            'uint_least8_t', 'uint_least16_t', 'uint_least32_t',
            'uint_least64_t', 'int_fast8_t', 'int_fast16_t', 'int_fast32_t',
            'int_fast64_t', 'uint_fast8_t', 'uint_fast16_t', 'uint_fast32_t',
            'uint_fast64_t', 'intptr_t', 'uintptr_t', 'intmax_t', 'uintmax_t']

    def __init__(self, **options):
        self.stdlibhighlighting = get_bool_opt(options,
                'stdlibhighlighting', True)
        self.c99highlighting = get_bool_opt(options,
                'c99highlighting', True)
        RegexLexer.__init__(self, **options)

    def get_tokens_unprocessed(self, text):
        for index, token, value in \
            RegexLexer.get_tokens_unprocessed(self, text):
            if token is Name:
                if self.stdlibhighlighting and value in self.stdlib_types:
                    token = Keyword.Type
                elif self.c99highlighting and value in self.c99_types:
                    token = Keyword.Type
            yield index, token, value

class CppLexer(RegexLexer):
    """
    For C++ source code with preprocessor directives.
    """
    name = 'C++'
    aliases = ['cpp', 'c++']
    filenames = ['*.cpp', '*.hpp', '*.c++', '*.h++',
                 '*.cc', '*.hh', '*.cxx', '*.hxx']
    mimetypes = ['text/x-c++hdr', 'text/x-c++src']

    #: optional Comment or Whitespace
    _ws = r'(?:\s|//.*?\n|/[*].*?[*]/)+'
    #: only one /* */ style comment
    _ws1 = r':\s*/[*].*?[*]/\s*'

    tokens = {
        'root': [
            # preprocessor directives: without whitespace
            ('^#if\s+0', Comment.Preproc, 'if0'),
            ('^#', Comment.Preproc, 'macro'),
            # or with whitespace
            ('^(' + _ws1 + r')(#if\s+0)',
             bygroups(using(this), Comment.Preproc), 'if0'),
            ('^(' + _ws1 + ')(#)',
             bygroups(using(this), Comment.Preproc), 'macro'),
            (r'\n', Text),
            (r'\s+', Text),
            (r'\\\n', Text), # line continuation
            (r'/(\\\n)?/(\n|(.|\n)*?[^\\]\n)', Comment.Single),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment.Multiline),
            (r'[{}]', Punctuation),
            (r'L?"', String, 'string'),
            (r"L?'(\\.|\\[0-7]{1,3}|\\x[a-fA-F0-9]{1,2}|[^\\\'\n])'", String.Char),
            (r'(\d+\.\d*|\.\d+|\d+)[eE][+-]?\d+[LlUu]*', Number.Float),
            (r'(\d+\.\d*|\.\d+|\d+[fF])[fF]?', Number.Float),
            (r'0x[0-9a-fA-F]+[LlUu]*', Number.Hex),
            (r'0[0-7]+[LlUu]*', Number.Oct),
            (r'\d+[LlUu]*', Number.Integer),
            (r'\*/', Error),
            (r'[~!%^&*+=|?:<>/-]', Operator),
            (r'[()\[\],.;]', Punctuation),
            (r'(asm|auto|break|case|catch|const|const_cast|continue|'
             r'default|delete|do|dynamic_cast|else|enum|explicit|export|'
             r'extern|for|friend|goto|if|mutable|namespace|new|operator|'
             r'private|protected|public|register|reinterpret_cast|return|'
             r'restrict|sizeof|static|static_cast|struct|switch|template|'
             r'this|throw|throws|try|typedef|typeid|typename|union|using|'
             r'volatile|virtual|while)\b', Keyword),
            (r'(class)(\s+)', bygroups(Keyword, Text), 'classname'),
            (r'(bool|int|long|float|short|double|char|unsigned|signed|'
             r'void|wchar_t)\b', Keyword.Type),
            (r'(_{0,2}inline|naked|thread)\b', Keyword.Reserved),
            (r'__(asm|int8|based|except|int16|stdcall|cdecl|fastcall|int32|'
             r'declspec|finally|int64|try|leave|wchar_t|w64|virtual_inheritance|'
             r'uuidof|unaligned|super|single_inheritance|raise|noop|'
             r'multiple_inheritance|m128i|m128d|m128|m64|interface|'
             r'identifier|forceinline|event|assume)\b', Keyword.Reserved),
            # Offload C++ extensions, http://offload.codeplay.com/
            (r'(__offload|__blockingoffload|__outer)\b', Keyword.Pseudo),
            (r'(true|false)\b', Keyword.Constant),
            (r'NULL\b', Name.Builtin),
            ('[a-zA-Z_][a-zA-Z0-9_]*:(?!:)', Name.Label),
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name),
        ],
        'classname': [
            (r'[a-zA-Z_][a-zA-Z0-9_]*', Name.Class, '#pop'),
            # template specification
            (r'\s*(?=>)', Text, '#pop'),
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'\\([\\abfnrtv"\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})', String.Escape),
            (r'[^\\"\n]+', String), # all other characters
            (r'\\\n', String), # line continuation
            (r'\\', String), # stray backslash
        ],
        'macro': [
            (r'[^/\n]+', Comment.Preproc),
            (r'/[*](.|\n)*?[*]/', Comment.Multiline),
            (r'//.*?\n', Comment.Single, '#pop'),
            (r'/', Comment.Preproc),
            (r'(?<=\\)\n', Comment.Preproc),
            (r'\n', Comment.Preproc, '#pop'),
        ],
        'if0': [
            (r'^\s*#if.*?(?<!\\)\n', Comment.Preproc, '#push'),
            (r'^\s*#endif.*?(?<!\\)\n', Comment.Preproc, '#pop'),
            (r'.*?\n', Comment),
        ]
    }


class ECLexer(RegexLexer):
    """
    For eC source code with preprocessor directives.

    *New in Pygments 1.5.*
    """
    name = 'eC'
    aliases = ['ec']
    filenames = ['*.ec', '*.eh']
    mimetypes = ['text/x-echdr', 'text/x-ecsrc']

    #: optional Comment or Whitespace
    _ws = r'(?:\s|//.*?\n|/[*].*?[*]/)+'
    #: only one /* */ style comment
    _ws1 = r':\s*/[*].*?[*]/\s*'

    tokens = {
        'whitespace': [
            # preprocessor directives: without whitespace
            ('^#if\s+0', Comment.Preproc, 'if0'),
            ('^#', Comment.Preproc, 'macro'),
            # or with whitespace
            ('^' + _ws1 + r'#if\s+0', Comment.Preproc, 'if0'),
            ('^' + _ws1 + '#', Comment.Preproc, 'macro'),
            (r'^(\s*)([a-zA-Z_][a-zA-Z0-9_]*:(?!:))', bygroups(Text, Name.Label)),
            (r'\n', Text),
            (r'\s+', Text),
            (r'\\\n', Text), # line continuation
            (r'//(\n|(.|\n)*?[^\\]\n)', Comment.Single),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment.Multiline),
        ],
        'statements': [
            (r'L?"', String, 'string'),
            (r"L?'(\\.|\\[0-7]{1,3}|\\x[a-fA-F0-9]{1,2}|[^\\\'\n])'", String.Char),
            (r'(\d+\.\d*|\.\d+|\d+)[eE][+-]?\d+[LlUu]*', Number.Float),
            (r'(\d+\.\d*|\.\d+|\d+[fF])[fF]?', Number.Float),
            (r'0x[0-9a-fA-F]+[LlUu]*', Number.Hex),
            (r'0[0-7]+[LlUu]*', Number.Oct),
            (r'\d+[LlUu]*', Number.Integer),
            (r'\*/', Error),
            (r'[~!%^&*+=|?:<>/-]', Operator),
            (r'[()\[\],.]', Punctuation),
            (r'\b(case)(.+?)(:)', bygroups(Keyword, using(this), Text)),
            (r'(auto|break|case|const|continue|default|do|else|enum|extern|'
             r'for|goto|if|register|restricted|return|sizeof|static|struct|'
             r'switch|typedef|union|volatile|virtual|while|class|private|public|'
             r'property|import|delete|new|new0|renew|renew0|define|get|set|remote|dllexport|dllimport|stdcall|'
             r'subclass|__on_register_module|namespace|using|typed_object|any_object|incref|register|watch|'
             r'stopwatching|firewatchers|watchable|class_designer|class_fixed|class_no_expansion|isset|'
             r'class_default_property|property_category|class_data|class_property|virtual|thisclass|'
             r'dbtable|dbindex|database_open|dbfield)\b', Keyword),
            (r'(int|long|float|short|double|char|unsigned|signed|void)\b',
             Keyword.Type),
            (r'(uint|uint16|uint32|uint64|bool|byte|unichar|int64)\b',
             Keyword.Type),
            (r'(class)(\s+)', bygroups(Keyword, Text), 'classname'),
            (r'(_{0,2}inline|naked|restrict|thread|typename)\b', Keyword.Reserved),
            (r'__(asm|int8|based|except|int16|stdcall|cdecl|fastcall|int32|'
             r'declspec|finally|int64|try|leave)\b', Keyword.Reserved),
            (r'(true|false|null|value|this|NULL)\b', Name.Builtin),
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name),
        ],
        'root': [
            include('whitespace'),
            # functions
            (r'((?:[a-zA-Z0-9_*\s])+?(?:\s|[*]))'    # return arguments
             r'([a-zA-Z_][a-zA-Z0-9_]*)'             # method name
             r'(\s*\([^;]*?\))'                      # signature
             r'(' + _ws + r')?({)',
             bygroups(using(this), Name.Function, using(this), using(this),
                      Punctuation),
             'function'),
            # function declarations
            (r'((?:[a-zA-Z0-9_*\s])+?(?:\s|[*]))'    # return arguments
             r'([a-zA-Z_][a-zA-Z0-9_]*)'             # method name
             r'(\s*\([^;]*?\))'                      # signature
             r'(' + _ws + r')?(;)',
             bygroups(using(this), Name.Function, using(this), using(this),
                      Punctuation)),
            ('', Text, 'statement'),
        ],
        'classname': [
            (r'[a-zA-Z_][a-zA-Z0-9_]*', Name.Class, '#pop'),
            # template specification
            (r'\s*(?=>)', Text, '#pop'),
        ],
        'statement' : [
            include('whitespace'),
            include('statements'),
            ('[{}]', Punctuation),
            (';', Punctuation, '#pop'),
        ],
        'function': [
            include('whitespace'),
            include('statements'),
            (';', Punctuation),
            ('{', Punctuation, '#push'),
            ('}', Punctuation, '#pop'),
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'\\([\\abfnrtv"\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})', String.Escape),
            (r'[^\\"\n]+', String), # all other characters
            (r'\\\n', String), # line continuation
            (r'\\', String), # stray backslash
        ],
        'macro': [
            (r'[^/\n]+', Comment.Preproc),
            (r'/[*](.|\n)*?[*]/', Comment.Multiline),
            (r'//.*?\n', Comment.Single, '#pop'),
            (r'/', Comment.Preproc),
            (r'(?<=\\)\n', Comment.Preproc),
            (r'\n', Comment.Preproc, '#pop'),
        ],
        'if0': [
            (r'^\s*#if.*?(?<!\\)\n', Comment.Preproc, '#push'),
            (r'^\s*#el(?:se|if).*\n', Comment.Preproc, '#pop'),
            (r'^\s*#endif.*?(?<!\\)\n', Comment.Preproc, '#pop'),
            (r'.*?\n', Comment),
        ]
    }

    stdlib_types = ['size_t', 'ssize_t', 'off_t', 'wchar_t', 'ptrdiff_t',
            'sig_atomic_t', 'fpos_t', 'clock_t', 'time_t', 'va_list',
            'jmp_buf', 'FILE', 'DIR', 'div_t', 'ldiv_t', 'mbstate_t',
            'wctrans_t', 'wint_t', 'wctype_t']
    c99_types = ['_Bool', '_Complex', 'int8_t', 'int16_t', 'int32_t', 'int64_t',
            'uint8_t', 'uint16_t', 'uint32_t', 'uint64_t', 'int_least8_t',
            'int_least16_t', 'int_least32_t', 'int_least64_t',
            'uint_least8_t', 'uint_least16_t', 'uint_least32_t',
            'uint_least64_t', 'int_fast8_t', 'int_fast16_t', 'int_fast32_t',
            'int_fast64_t', 'uint_fast8_t', 'uint_fast16_t', 'uint_fast32_t',
            'uint_fast64_t', 'intptr_t', 'uintptr_t', 'intmax_t', 'uintmax_t']

    def __init__(self, **options):
        self.stdlibhighlighting = get_bool_opt(options,
                'stdlibhighlighting', True)
        self.c99highlighting = get_bool_opt(options,
                'c99highlighting', True)
        RegexLexer.__init__(self, **options)

    def get_tokens_unprocessed(self, text):
        for index, token, value in \
            RegexLexer.get_tokens_unprocessed(self, text):
            if token is Name:
                if self.stdlibhighlighting and value in self.stdlib_types:
                    token = Keyword.Type
                elif self.c99highlighting and value in self.c99_types:
                    token = Keyword.Type
            yield index, token, value


class DLexer(RegexLexer):
    """
    For D source.

    *New in Pygments 1.2.*
    """
    name = 'D'
    filenames = ['*.d', '*.di']
    aliases = ['d']
    mimetypes = ['text/x-dsrc']

    tokens = {
        'root': [
            (r'\n', Text),
            (r'\s+', Text),
            #(r'\\\n', Text), # line continuations
            # Comments
            (r'//(.*?)\n', Comment.Single),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment.Multiline),
            (r'/\+', Comment.Multiline, 'nested_comment'),
            # Keywords
            (r'(abstract|alias|align|asm|assert|auto|body|break|case|cast'
             r'|catch|class|const|continue|debug|default|delegate|delete'
             r'|deprecated|do|else|enum|export|extern|finally|final'
             r'|foreach_reverse|foreach|for|function|goto|if|import|inout'
             r'|interface|invariant|in|is|lazy|mixin|module|new|nothrow|out'
             r'|override|package|pragma|private|protected|public|pure|ref|return'
             r'|scope|static|struct|super|switch|synchronized|template|this'
             r'|throw|try|typedef|typeid|typeof|union|unittest|version|volatile'
             r'|while|with|__traits)\b', Keyword
            ),
            (r'(bool|byte|cdouble|cent|cfloat|char|creal|dchar|double|float'
             r'|idouble|ifloat|int|ireal|long|real|short|ubyte|ucent|uint|ulong'
             r'|ushort|void|wchar)\b', Keyword.Type
            ),
            (r'(false|true|null)\b', Keyword.Constant),
            (r'macro\b', Keyword.Reserved),
            (r'(string|wstring|dstring)\b', Name.Builtin),
            # FloatLiteral
            # -- HexFloat
            (r'0[xX]([0-9a-fA-F_]*\.[0-9a-fA-F_]+|[0-9a-fA-F_]+)'
             r'[pP][+\-]?[0-9_]+[fFL]?[i]?', Number.Float),
            # -- DecimalFloat
            (r'[0-9_]+(\.[0-9_]+[eE][+\-]?[0-9_]+|'
             r'\.[0-9_]*|[eE][+\-]?[0-9_]+)[fFL]?[i]?', Number.Float),
            (r'\.(0|[1-9][0-9_]*)([eE][+\-]?[0-9_]+)?[fFL]?[i]?', Number.Float),
            # IntegerLiteral
            # -- Binary
            (r'0[Bb][01_]+', Number),
            # -- Octal
            (r'0[0-7_]+', Number.Oct),
            # -- Hexadecimal
            (r'0[xX][0-9a-fA-F_]+', Number.Hex),
            # -- Decimal
            (r'(0|[1-9][0-9_]*)([LUu]|Lu|LU|uL|UL)?', Number.Integer),
            # CharacterLiteral
            (r"""'(\\['"?\\abfnrtv]|\\x[0-9a-fA-F]{2}|\\[0-7]{1,3}"""
             r"""|\\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8}|\\&\w+;|.)'""",
             String.Char
            ),
            # StringLiteral
            # -- WysiwygString
            (r'r"[^"]*"[cwd]?', String),
            # -- AlternateWysiwygString
            (r'`[^`]*`[cwd]?', String),
            # -- DoubleQuotedString
            (r'"(\\\\|\\"|[^"])*"[cwd]?', String),
            # -- EscapeSequence
            (r"\\(['\"?\\abfnrtv]|x[0-9a-fA-F]{2}|[0-7]{1,3}"
             r"|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8}|&\w+;)",
             String
            ),
            # -- HexString
            (r'x"[0-9a-fA-F_\s]*"[cwd]?', String),
            # -- DelimitedString
            (r'q"\[', String, 'delimited_bracket'),
            (r'q"\(', String, 'delimited_parenthesis'),
            (r'q"<', String, 'delimited_angle'),
            (r'q"{', String, 'delimited_curly'),
            (r'q"([a-zA-Z_]\w*)\n.*?\n\1"', String),
            (r'q"(.).*?\1"', String),
            # -- TokenString
            (r'q{', String, 'token_string'),
            # Tokens
            (r'(~=|\^=|%=|\*=|==|!>=|!<=|!<>=|!<>|!<|!>|!=|>>>=|>>>|>>=|>>|>='
             r'|<>=|<>|<<=|<<|<=|\+\+|\+=|--|-=|\|\||\|=|&&|&=|\.\.\.|\.\.|/=)'
             r'|[/.&|\-+<>!()\[\]{}?,;:$=*%^~]', Punctuation
            ),
            # Identifier
            (r'[a-zA-Z_]\w*', Name),
        ],
        'nested_comment': [
            (r'[^+/]+', Comment.Multiline),
            (r'/\+', Comment.Multiline, '#push'),
            (r'\+/', Comment.Multiline, '#pop'),
            (r'[+/]', Comment.Multiline),
        ],
        'token_string': [
            (r'{', Punctuation, 'token_string_nest'),
            (r'}', String, '#pop'),
            include('root'),
        ],
        'token_string_nest': [
            (r'{', Punctuation, '#push'),
            (r'}', Punctuation, '#pop'),
            include('root'),
        ],
        'delimited_bracket': [
            (r'[^\[\]]+', String),
            (r'\[', String, 'delimited_inside_bracket'),
            (r'\]"', String, '#pop'),
        ],
        'delimited_inside_bracket': [
            (r'[^\[\]]+', String),
            (r'\[', String, '#push'),
            (r'\]', String, '#pop'),
        ],
        'delimited_parenthesis': [
            (r'[^\(\)]+', String),
            (r'\(', String, 'delimited_inside_parenthesis'),
            (r'\)"', String, '#pop'),
        ],
        'delimited_inside_parenthesis': [
            (r'[^\(\)]+', String),
            (r'\(', String, '#push'),
            (r'\)', String, '#pop'),
        ],
        'delimited_angle': [
            (r'[^<>]+', String),
            (r'<', String, 'delimited_inside_angle'),
            (r'>"', String, '#pop'),
        ],
        'delimited_inside_angle': [
            (r'[^<>]+', String),
            (r'<', String, '#push'),
            (r'>', String, '#pop'),
        ],
        'delimited_curly': [
            (r'[^{}]+', String),
            (r'{', String, 'delimited_inside_curly'),
            (r'}"', String, '#pop'),
        ],
        'delimited_inside_curly': [
            (r'[^{}]+', String),
            (r'{', String, '#push'),
            (r'}', String, '#pop'),
        ],
    }


class DelphiLexer(Lexer):
    """
    For `Delphi <http://www.borland.com/delphi/>`_ (Borland Object Pascal),
    Turbo Pascal and Free Pascal source code.

    Additional options accepted:

    `turbopascal`
        Highlight Turbo Pascal specific keywords (default: ``True``).
    `delphi`
        Highlight Borland Delphi specific keywords (default: ``True``).
    `freepascal`
        Highlight Free Pascal specific keywords (default: ``True``).
    `units`
        A list of units that should be considered builtin, supported are
        ``System``, ``SysUtils``, ``Classes`` and ``Math``.
        Default is to consider all of them builtin.
    """
    name = 'Delphi'
    aliases = ['delphi', 'pas', 'pascal', 'objectpascal']
    filenames = ['*.pas']
    mimetypes = ['text/x-pascal']

    TURBO_PASCAL_KEYWORDS = [
        'absolute', 'and', 'array', 'asm', 'begin', 'break', 'case',
        'const', 'constructor', 'continue', 'destructor', 'div', 'do',
        'downto', 'else', 'end', 'file', 'for', 'function', 'goto',
        'if', 'implementation', 'in', 'inherited', 'inline', 'interface',
        'label', 'mod', 'nil', 'not', 'object', 'of', 'on', 'operator',
        'or', 'packed', 'procedure', 'program', 'record', 'reintroduce',
        'repeat', 'self', 'set', 'shl', 'shr', 'string', 'then', 'to',
        'type', 'unit', 'until', 'uses', 'var', 'while', 'with', 'xor'
    ]

    DELPHI_KEYWORDS = [
        'as', 'class', 'except', 'exports', 'finalization', 'finally',
        'initialization', 'is', 'library', 'on', 'property', 'raise',
        'threadvar', 'try'
    ]

    FREE_PASCAL_KEYWORDS = [
        'dispose', 'exit', 'false', 'new', 'true'
    ]

    BLOCK_KEYWORDS = set([
        'begin', 'class', 'const', 'constructor', 'destructor', 'end',
        'finalization', 'function', 'implementation', 'initialization',
        'label', 'library', 'operator', 'procedure', 'program', 'property',
        'record', 'threadvar', 'type', 'unit', 'uses', 'var'
    ])

    FUNCTION_MODIFIERS = set([
        'alias', 'cdecl', 'export', 'inline', 'interrupt', 'nostackframe',
        'pascal', 'register', 'safecall', 'softfloat', 'stdcall',
        'varargs', 'name', 'dynamic', 'near', 'virtual', 'external',
        'override', 'assembler'
    ])

    # XXX: those aren't global. but currently we know no way for defining
    #      them just for the type context.
    DIRECTIVES = set([
        'absolute', 'abstract', 'assembler', 'cppdecl', 'default', 'far',
        'far16', 'forward', 'index', 'oldfpccall', 'private', 'protected',
        'published', 'public'
    ])

    BUILTIN_TYPES = set([
        'ansichar', 'ansistring', 'bool', 'boolean', 'byte', 'bytebool',
        'cardinal', 'char', 'comp', 'currency', 'double', 'dword',
        'extended', 'int64', 'integer', 'iunknown', 'longbool', 'longint',
        'longword', 'pansichar', 'pansistring', 'pbool', 'pboolean',
        'pbyte', 'pbytearray', 'pcardinal', 'pchar', 'pcomp', 'pcurrency',
        'pdate', 'pdatetime', 'pdouble', 'pdword', 'pextended', 'phandle',
        'pint64', 'pinteger', 'plongint', 'plongword', 'pointer',
        'ppointer', 'pshortint', 'pshortstring', 'psingle', 'psmallint',
        'pstring', 'pvariant', 'pwidechar', 'pwidestring', 'pword',
        'pwordarray', 'pwordbool', 'real', 'real48', 'shortint',
        'shortstring', 'single', 'smallint', 'string', 'tclass', 'tdate',
        'tdatetime', 'textfile', 'thandle', 'tobject', 'ttime', 'variant',
        'widechar', 'widestring', 'word', 'wordbool'
    ])

    BUILTIN_UNITS = {
        'System': [
            'abs', 'acquireexceptionobject', 'addr', 'ansitoutf8',
            'append', 'arctan', 'assert', 'assigned', 'assignfile',
            'beginthread', 'blockread', 'blockwrite', 'break', 'chdir',
            'chr', 'close', 'closefile', 'comptocurrency', 'comptodouble',
            'concat', 'continue', 'copy', 'cos', 'dec', 'delete',
            'dispose', 'doubletocomp', 'endthread', 'enummodules',
            'enumresourcemodules', 'eof', 'eoln', 'erase', 'exceptaddr',
            'exceptobject', 'exclude', 'exit', 'exp', 'filepos', 'filesize',
            'fillchar', 'finalize', 'findclasshinstance', 'findhinstance',
            'findresourcehinstance', 'flush', 'frac', 'freemem',
            'get8087cw', 'getdir', 'getlasterror', 'getmem',
            'getmemorymanager', 'getmodulefilename', 'getvariantmanager',
            'halt', 'hi', 'high', 'inc', 'include', 'initialize', 'insert',
            'int', 'ioresult', 'ismemorymanagerset', 'isvariantmanagerset',
            'length', 'ln', 'lo', 'low', 'mkdir', 'move', 'new', 'odd',
            'olestrtostring', 'olestrtostrvar', 'ord', 'paramcount',
            'paramstr', 'pi', 'pos', 'pred', 'ptr', 'pucs4chars', 'random',
            'randomize', 'read', 'readln', 'reallocmem',
            'releaseexceptionobject', 'rename', 'reset', 'rewrite', 'rmdir',
            'round', 'runerror', 'seek', 'seekeof', 'seekeoln',
            'set8087cw', 'setlength', 'setlinebreakstyle',
            'setmemorymanager', 'setstring', 'settextbuf',
            'setvariantmanager', 'sin', 'sizeof', 'slice', 'sqr', 'sqrt',
            'str', 'stringofchar', 'stringtoolestr', 'stringtowidechar',
            'succ', 'swap', 'trunc', 'truncate', 'typeinfo',
            'ucs4stringtowidestring', 'unicodetoutf8', 'uniquestring',
            'upcase', 'utf8decode', 'utf8encode', 'utf8toansi',
            'utf8tounicode', 'val', 'vararrayredim', 'varclear',
            'widecharlentostring', 'widecharlentostrvar',
            'widechartostring', 'widechartostrvar',
            'widestringtoucs4string', 'write', 'writeln'
        ],
        'SysUtils': [
            'abort', 'addexitproc', 'addterminateproc', 'adjustlinebreaks',
            'allocmem', 'ansicomparefilename', 'ansicomparestr',
            'ansicomparetext', 'ansidequotedstr', 'ansiextractquotedstr',
            'ansilastchar', 'ansilowercase', 'ansilowercasefilename',
            'ansipos', 'ansiquotedstr', 'ansisamestr', 'ansisametext',
            'ansistrcomp', 'ansistricomp', 'ansistrlastchar', 'ansistrlcomp',
            'ansistrlicomp', 'ansistrlower', 'ansistrpos', 'ansistrrscan',
            'ansistrscan', 'ansistrupper', 'ansiuppercase',
            'ansiuppercasefilename', 'appendstr', 'assignstr', 'beep',
            'booltostr', 'bytetocharindex', 'bytetocharlen', 'bytetype',
            'callterminateprocs', 'changefileext', 'charlength',
            'chartobyteindex', 'chartobytelen', 'comparemem', 'comparestr',
            'comparetext', 'createdir', 'createguid', 'currentyear',
            'currtostr', 'currtostrf', 'date', 'datetimetofiledate',
            'datetimetostr', 'datetimetostring', 'datetimetosystemtime',
            'datetimetotimestamp', 'datetostr', 'dayofweek', 'decodedate',
            'decodedatefully', 'decodetime', 'deletefile', 'directoryexists',
            'diskfree', 'disksize', 'disposestr', 'encodedate', 'encodetime',
            'exceptionerrormessage', 'excludetrailingbackslash',
            'excludetrailingpathdelimiter', 'expandfilename',
            'expandfilenamecase', 'expanduncfilename', 'extractfiledir',
            'extractfiledrive', 'extractfileext', 'extractfilename',
            'extractfilepath', 'extractrelativepath', 'extractshortpathname',
            'fileage', 'fileclose', 'filecreate', 'filedatetodatetime',
            'fileexists', 'filegetattr', 'filegetdate', 'fileisreadonly',
            'fileopen', 'fileread', 'filesearch', 'fileseek', 'filesetattr',
            'filesetdate', 'filesetreadonly', 'filewrite', 'finalizepackage',
            'findclose', 'findcmdlineswitch', 'findfirst', 'findnext',
            'floattocurr', 'floattodatetime', 'floattodecimal', 'floattostr',
            'floattostrf', 'floattotext', 'floattotextfmt', 'fmtloadstr',
            'fmtstr', 'forcedirectories', 'format', 'formatbuf', 'formatcurr',
            'formatdatetime', 'formatfloat', 'freeandnil', 'getcurrentdir',
            'getenvironmentvariable', 'getfileversion', 'getformatsettings',
            'getlocaleformatsettings', 'getmodulename', 'getpackagedescription',
            'getpackageinfo', 'gettime', 'guidtostring', 'incamonth',
            'includetrailingbackslash', 'includetrailingpathdelimiter',
            'incmonth', 'initializepackage', 'interlockeddecrement',
            'interlockedexchange', 'interlockedexchangeadd',
            'interlockedincrement', 'inttohex', 'inttostr', 'isdelimiter',
            'isequalguid', 'isleapyear', 'ispathdelimiter', 'isvalidident',
            'languages', 'lastdelimiter', 'loadpackage', 'loadstr',
            'lowercase', 'msecstotimestamp', 'newstr', 'nextcharindex', 'now',
            'outofmemoryerror', 'quotedstr', 'raiselastoserror',
            'raiselastwin32error', 'removedir', 'renamefile', 'replacedate',
            'replacetime', 'safeloadlibrary', 'samefilename', 'sametext',
            'setcurrentdir', 'showexception', 'sleep', 'stralloc', 'strbufsize',
            'strbytetype', 'strcat', 'strcharlength', 'strcomp', 'strcopy',
            'strdispose', 'strecopy', 'strend', 'strfmt', 'stricomp',
            'stringreplace', 'stringtoguid', 'strlcat', 'strlcomp', 'strlcopy',
            'strlen', 'strlfmt', 'strlicomp', 'strlower', 'strmove', 'strnew',
            'strnextchar', 'strpas', 'strpcopy', 'strplcopy', 'strpos',
            'strrscan', 'strscan', 'strtobool', 'strtobooldef', 'strtocurr',
            'strtocurrdef', 'strtodate', 'strtodatedef', 'strtodatetime',
            'strtodatetimedef', 'strtofloat', 'strtofloatdef', 'strtoint',
            'strtoint64', 'strtoint64def', 'strtointdef', 'strtotime',
            'strtotimedef', 'strupper', 'supports', 'syserrormessage',
            'systemtimetodatetime', 'texttofloat', 'time', 'timestamptodatetime',
            'timestamptomsecs', 'timetostr', 'trim', 'trimleft', 'trimright',
            'tryencodedate', 'tryencodetime', 'tryfloattocurr', 'tryfloattodatetime',
            'trystrtobool', 'trystrtocurr', 'trystrtodate', 'trystrtodatetime',
            'trystrtofloat', 'trystrtoint', 'trystrtoint64', 'trystrtotime',
            'unloadpackage', 'uppercase', 'widecomparestr', 'widecomparetext',
            'widefmtstr', 'wideformat', 'wideformatbuf', 'widelowercase',
            'widesamestr', 'widesametext', 'wideuppercase', 'win32check',
            'wraptext'
        ],
        'Classes': [
            'activateclassgroup', 'allocatehwnd', 'bintohex', 'checksynchronize',
            'collectionsequal', 'countgenerations', 'deallocatehwnd', 'equalrect',
            'extractstrings', 'findclass', 'findglobalcomponent', 'getclass',
            'groupdescendantswith', 'hextobin', 'identtoint',
            'initinheritedcomponent', 'inttoident', 'invalidpoint',
            'isuniqueglobalcomponentname', 'linestart', 'objectbinarytotext',
            'objectresourcetotext', 'objecttexttobinary', 'objecttexttoresource',
            'pointsequal', 'readcomponentres', 'readcomponentresex',
            'readcomponentresfile', 'rect', 'registerclass', 'registerclassalias',
            'registerclasses', 'registercomponents', 'registerintegerconsts',
            'registernoicon', 'registernonactivex', 'smallpoint', 'startclassgroup',
            'teststreamformat', 'unregisterclass', 'unregisterclasses',
            'unregisterintegerconsts', 'unregistermoduleclasses',
            'writecomponentresfile'
        ],
        'Math': [
            'arccos', 'arccosh', 'arccot', 'arccoth', 'arccsc', 'arccsch', 'arcsec',
            'arcsech', 'arcsin', 'arcsinh', 'arctan2', 'arctanh', 'ceil',
            'comparevalue', 'cosecant', 'cosh', 'cot', 'cotan', 'coth', 'csc',
            'csch', 'cycletodeg', 'cycletograd', 'cycletorad', 'degtocycle',
            'degtograd', 'degtorad', 'divmod', 'doubledecliningbalance',
            'ensurerange', 'floor', 'frexp', 'futurevalue', 'getexceptionmask',
            'getprecisionmode', 'getroundmode', 'gradtocycle', 'gradtodeg',
            'gradtorad', 'hypot', 'inrange', 'interestpayment', 'interestrate',
            'internalrateofreturn', 'intpower', 'isinfinite', 'isnan', 'iszero',
            'ldexp', 'lnxp1', 'log10', 'log2', 'logn', 'max', 'maxintvalue',
            'maxvalue', 'mean', 'meanandstddev', 'min', 'minintvalue', 'minvalue',
            'momentskewkurtosis', 'netpresentvalue', 'norm', 'numberofperiods',
            'payment', 'periodpayment', 'poly', 'popnstddev', 'popnvariance',
            'power', 'presentvalue', 'radtocycle', 'radtodeg', 'radtograd',
            'randg', 'randomrange', 'roundto', 'samevalue', 'sec', 'secant',
            'sech', 'setexceptionmask', 'setprecisionmode', 'setroundmode',
            'sign', 'simpleroundto', 'sincos', 'sinh', 'slndepreciation', 'stddev',
            'sum', 'sumint', 'sumofsquares', 'sumsandsquares', 'syddepreciation',
            'tan', 'tanh', 'totalvariance', 'variance'
        ]
    }

    ASM_REGISTERS = set([
        'ah', 'al', 'ax', 'bh', 'bl', 'bp', 'bx', 'ch', 'cl', 'cr0',
        'cr1', 'cr2', 'cr3', 'cr4', 'cs', 'cx', 'dh', 'di', 'dl', 'dr0',
        'dr1', 'dr2', 'dr3', 'dr4', 'dr5', 'dr6', 'dr7', 'ds', 'dx',
        'eax', 'ebp', 'ebx', 'ecx', 'edi', 'edx', 'es', 'esi', 'esp',
        'fs', 'gs', 'mm0', 'mm1', 'mm2', 'mm3', 'mm4', 'mm5', 'mm6',
        'mm7', 'si', 'sp', 'ss', 'st0', 'st1', 'st2', 'st3', 'st4', 'st5',
        'st6', 'st7', 'xmm0', 'xmm1', 'xmm2', 'xmm3', 'xmm4', 'xmm5',
        'xmm6', 'xmm7'
    ])

    ASM_INSTRUCTIONS = set([
        'aaa', 'aad', 'aam', 'aas', 'adc', 'add', 'and', 'arpl', 'bound',
        'bsf', 'bsr', 'bswap', 'bt', 'btc', 'btr', 'bts', 'call', 'cbw',
        'cdq', 'clc', 'cld', 'cli', 'clts', 'cmc', 'cmova', 'cmovae',
        'cmovb', 'cmovbe', 'cmovc', 'cmovcxz', 'cmove', 'cmovg',
        'cmovge', 'cmovl', 'cmovle', 'cmovna', 'cmovnae', 'cmovnb',
        'cmovnbe', 'cmovnc', 'cmovne', 'cmovng', 'cmovnge', 'cmovnl',
        'cmovnle', 'cmovno', 'cmovnp', 'cmovns', 'cmovnz', 'cmovo',
        'cmovp', 'cmovpe', 'cmovpo', 'cmovs', 'cmovz', 'cmp', 'cmpsb',
        'cmpsd', 'cmpsw', 'cmpxchg', 'cmpxchg486', 'cmpxchg8b', 'cpuid',
        'cwd', 'cwde', 'daa', 'das', 'dec', 'div', 'emms', 'enter', 'hlt',
        'ibts', 'icebp', 'idiv', 'imul', 'in', 'inc', 'insb', 'insd',
        'insw', 'int', 'int01', 'int03', 'int1', 'int3', 'into', 'invd',
        'invlpg', 'iret', 'iretd', 'iretw', 'ja', 'jae', 'jb', 'jbe',
        'jc', 'jcxz', 'jcxz', 'je', 'jecxz', 'jg', 'jge', 'jl', 'jle',
        'jmp', 'jna', 'jnae', 'jnb', 'jnbe', 'jnc', 'jne', 'jng', 'jnge',
        'jnl', 'jnle', 'jno', 'jnp', 'jns', 'jnz', 'jo', 'jp', 'jpe',
        'jpo', 'js', 'jz', 'lahf', 'lar', 'lcall', 'lds', 'lea', 'leave',
        'les', 'lfs', 'lgdt', 'lgs', 'lidt', 'ljmp', 'lldt', 'lmsw',
        'loadall', 'loadall286', 'lock', 'lodsb', 'lodsd', 'lodsw',
        'loop', 'loope', 'loopne', 'loopnz', 'loopz', 'lsl', 'lss', 'ltr',
        'mov', 'movd', 'movq', 'movsb', 'movsd', 'movsw', 'movsx',
        'movzx', 'mul', 'neg', 'nop', 'not', 'or', 'out', 'outsb', 'outsd',
        'outsw', 'pop', 'popa', 'popad', 'popaw', 'popf', 'popfd', 'popfw',
        'push', 'pusha', 'pushad', 'pushaw', 'pushf', 'pushfd', 'pushfw',
        'rcl', 'rcr', 'rdmsr', 'rdpmc', 'rdshr', 'rdtsc', 'rep', 'repe',
        'repne', 'repnz', 'repz', 'ret', 'retf', 'retn', 'rol', 'ror',
        'rsdc', 'rsldt', 'rsm', 'sahf', 'sal', 'salc', 'sar', 'sbb',
        'scasb', 'scasd', 'scasw', 'seta', 'setae', 'setb', 'setbe',
        'setc', 'setcxz', 'sete', 'setg', 'setge', 'setl', 'setle',
        'setna', 'setnae', 'setnb', 'setnbe', 'setnc', 'setne', 'setng',
        'setnge', 'setnl', 'setnle', 'setno', 'setnp', 'setns', 'setnz',
        'seto', 'setp', 'setpe', 'setpo', 'sets', 'setz', 'sgdt', 'shl',
        'shld', 'shr', 'shrd', 'sidt', 'sldt', 'smi', 'smint', 'smintold',
        'smsw', 'stc', 'std', 'sti', 'stosb', 'stosd', 'stosw', 'str',
        'sub', 'svdc', 'svldt', 'svts', 'syscall', 'sysenter', 'sysexit',
        'sysret', 'test', 'ud1', 'ud2', 'umov', 'verr', 'verw', 'wait',
        'wbinvd', 'wrmsr', 'wrshr', 'xadd', 'xbts', 'xchg', 'xlat',
        'xlatb', 'xor'
    ])

    def __init__(self, **options):
        Lexer.__init__(self, **options)
        self.keywords = set()
        if get_bool_opt(options, 'turbopascal', True):
            self.keywords.update(self.TURBO_PASCAL_KEYWORDS)
        if get_bool_opt(options, 'delphi', True):
            self.keywords.update(self.DELPHI_KEYWORDS)
        if get_bool_opt(options, 'freepascal', True):
            self.keywords.update(self.FREE_PASCAL_KEYWORDS)
        self.builtins = set()
        for unit in get_list_opt(options, 'units', self.BUILTIN_UNITS.keys()):
            self.builtins.update(self.BUILTIN_UNITS[unit])

    def get_tokens_unprocessed(self, text):
        scanner = Scanner(text, re.DOTALL | re.MULTILINE | re.IGNORECASE)
        stack = ['initial']
        in_function_block = False
        in_property_block = False
        was_dot = False
        next_token_is_function = False
        next_token_is_property = False
        collect_labels = False
        block_labels = set()
        brace_balance = [0, 0]

        while not scanner.eos:
            token = Error

            if stack[-1] == 'initial':
                if scanner.scan(r'\s+'):
                    token = Text
                elif scanner.scan(r'\{.*?\}|\(\*.*?\*\)'):
                    if scanner.match.startswith('$'):
                        token = Comment.Preproc
                    else:
                        token = Comment.Multiline
                elif scanner.scan(r'//.*?$'):
                    token = Comment.Single
                elif scanner.scan(r'[-+*\/=<>:;,.@\^]'):
                    token = Operator
                    # stop label highlighting on next ";"
                    if collect_labels and scanner.match == ';':
                        collect_labels = False
                elif scanner.scan(r'[\(\)\[\]]+'):
                    token = Punctuation
                    # abort function naming ``foo = Function(...)``
                    next_token_is_function = False
                    # if we are in a function block we count the open
                    # braces because ootherwise it's impossible to
                    # determine the end of the modifier context
                    if in_function_block or in_property_block:
                        if scanner.match == '(':
                            brace_balance[0] += 1
                        elif scanner.match == ')':
                            brace_balance[0] -= 1
                        elif scanner.match == '[':
                            brace_balance[1] += 1
                        elif scanner.match == ']':
                            brace_balance[1] -= 1
                elif scanner.scan(r'[A-Za-z_][A-Za-z_0-9]*'):
                    lowercase_name = scanner.match.lower()
                    if lowercase_name == 'result':
                        token = Name.Builtin.Pseudo
                    elif lowercase_name in self.keywords:
                        token = Keyword
                        # if we are in a special block and a
                        # block ending keyword occours (and the parenthesis
                        # is balanced) we end the current block context
                        if (in_function_block or in_property_block) and \
                           lowercase_name in self.BLOCK_KEYWORDS and \
                           brace_balance[0] <= 0 and \
                           brace_balance[1] <= 0:
                            in_function_block = False
                            in_property_block = False
                            brace_balance = [0, 0]
                            block_labels = set()
                        if lowercase_name in ('label', 'goto'):
                            collect_labels = True
                        elif lowercase_name == 'asm':
                            stack.append('asm')
                        elif lowercase_name == 'property':
                            in_property_block = True
                            next_token_is_property = True
                        elif lowercase_name in ('procedure', 'operator',
                                                'function', 'constructor',
                                                'destructor'):
                            in_function_block = True
                            next_token_is_function = True
                    # we are in a function block and the current name
                    # is in the set of registered modifiers. highlight
                    # it as pseudo keyword
                    elif in_function_block and \
                         lowercase_name in self.FUNCTION_MODIFIERS:
                        token = Keyword.Pseudo
                    # if we are in a property highlight some more
                    # modifiers
                    elif in_property_block and \
                         lowercase_name in ('read', 'write'):
                        token = Keyword.Pseudo
                        next_token_is_function = True
                    # if the last iteration set next_token_is_function
                    # to true we now want this name highlighted as
                    # function. so do that and reset the state
                    elif next_token_is_function:
                        # Look if the next token is a dot. If yes it's
                        # not a function, but a class name and the
                        # part after the dot a function name
                        if scanner.test(r'\s*\.\s*'):
                            token = Name.Class
                        # it's not a dot, our job is done
                        else:
                            token = Name.Function
                            next_token_is_function = False
                    # same for properties
                    elif next_token_is_property:
                        token = Name.Property
                        next_token_is_property = False
                    # Highlight this token as label and add it
                    # to the list of known labels
                    elif collect_labels:
                        token = Name.Label
                        block_labels.add(scanner.match.lower())
                    # name is in list of known labels
                    elif lowercase_name in block_labels:
                        token = Name.Label
                    elif lowercase_name in self.BUILTIN_TYPES:
                        token = Keyword.Type
                    elif lowercase_name in self.DIRECTIVES:
                        token = Keyword.Pseudo
                    # builtins are just builtins if the token
                    # before isn't a dot
                    elif not was_dot and lowercase_name in self.builtins:
                        token = Name.Builtin
                    else:
                        token = Name
                elif scanner.scan(r"'"):
                    token = String
                    stack.append('string')
                elif scanner.scan(r'\#(\d+|\$[0-9A-Fa-f]+)'):
                    token = String.Char
                elif scanner.scan(r'\$[0-9A-Fa-f]+'):
                    token = Number.Hex
                elif scanner.scan(r'\d+(?![eE]|\.[^.])'):
                    token = Number.Integer
                elif scanner.scan(r'\d+(\.\d+([eE][+-]?\d+)?|[eE][+-]?\d+)'):
                    token = Number.Float
                else:
                    # if the stack depth is deeper than once, pop
                    if len(stack) > 1:
                        stack.pop()
                    scanner.get_char()

            elif stack[-1] == 'string':
                if scanner.scan(r"''"):
                    token = String.Escape
                elif scanner.scan(r"'"):
                    token = String
                    stack.pop()
                elif scanner.scan(r"[^']*"):
                    token = String
                else:
                    scanner.get_char()
                    stack.pop()

            elif stack[-1] == 'asm':
                if scanner.scan(r'\s+'):
                    token = Text
                elif scanner.scan(r'end'):
                    token = Keyword
                    stack.pop()
                elif scanner.scan(r'\{.*?\}|\(\*.*?\*\)'):
                    if scanner.match.startswith('$'):
                        token = Comment.Preproc
                    else:
                        token = Comment.Multiline
                elif scanner.scan(r'//.*?$'):
                    token = Comment.Single
                elif scanner.scan(r"'"):
                    token = String
                    stack.append('string')
                elif scanner.scan(r'@@[A-Za-z_][A-Za-z_0-9]*'):
                    token = Name.Label
                elif scanner.scan(r'[A-Za-z_][A-Za-z_0-9]*'):
                    lowercase_name = scanner.match.lower()
                    if lowercase_name in self.ASM_INSTRUCTIONS:
                        token = Keyword
                    elif lowercase_name in self.ASM_REGISTERS:
                        token = Name.Builtin
                    else:
                        token = Name
                elif scanner.scan(r'[-+*\/=<>:;,.@\^]+'):
                    token = Operator
                elif scanner.scan(r'[\(\)\[\]]+'):
                    token = Punctuation
                elif scanner.scan(r'\$[0-9A-Fa-f]+'):
                    token = Number.Hex
                elif scanner.scan(r'\d+(?![eE]|\.[^.])'):
                    token = Number.Integer
                elif scanner.scan(r'\d+(\.\d+([eE][+-]?\d+)?|[eE][+-]?\d+)'):
                    token = Number.Float
                else:
                    scanner.get_char()
                    stack.pop()

            # save the dot!!!11
            if scanner.match.strip():
                was_dot = scanner.match == '.'
            yield scanner.start_pos, token, scanner.match or ''


class DylanLexer(RegexLexer):
    """
    For the `Dylan <http://www.opendylan.org/>`_ language.

    *New in Pygments 0.7.*
    """

    name = 'Dylan'
    aliases = ['dylan']
    filenames = ['*.dylan', '*.dyl']
    mimetypes = ['text/x-dylan']

    flags = re.DOTALL

    tokens = {
        'root': [
            (r'\b(subclass|abstract|block|c(on(crete|stant)|lass)|domain'
             r'|ex(c(eption|lude)|port)|f(unction(al)?)|generic|handler'
             r'|i(n(herited|line|stance|terface)|mport)|library|m(acro|ethod)'
             r'|open|primary|sealed|si(deways|ngleton)|slot'
             r'|v(ariable|irtual))\b', Name.Builtin),
            (r'<\w+>', Keyword.Type),
            (r'//.*?\n', Comment.Single),
            (r'/\*[\w\W]*?\*/', Comment.Multiline),
            (r'"', String, 'string'),
            (r"'(\\.|\\[0-7]{1,3}|\\x[a-fA-F0-9]{1,2}|[^\\\'\n])'", String.Char),
            (r'=>|\b(a(bove|fterwards)|b(e(gin|low)|y)|c(ase|leanup|reate)'
             r'|define|else(if)?|end|f(inally|or|rom)|i[fn]|l(et|ocal)|otherwise'
             r'|rename|s(elect|ignal)|t(hen|o)|u(n(less|til)|se)|wh(en|ile))\b',
             Keyword),
            (r'([ \t])([!\$%&\*\/:<=>\?~_^a-zA-Z0-9.+\-]*:)',
             bygroups(Text, Name.Variable)),
            (r'([ \t]*)(\S+[^:])([ \t]*)(\()([ \t]*)',
             bygroups(Text, Name.Function, Text, Punctuation, Text)),
            (r'-?[0-9.]+', Number),
            (r'[(),;]', Punctuation),
            (r'\$[a-zA-Z0-9-]+', Name.Constant),
            (r'[!$%&*/:<>=?~^.+\[\]{}-]+', Operator),
            (r'\s+', Text),
            (r'#"[a-zA-Z0-9-]+"', Keyword),
            (r'#[a-zA-Z0-9-]+', Keyword),
            (r'#(\(|\[)', Punctuation),
            (r'[a-zA-Z0-9-_]+', Name.Variable),
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'\\([\\abfnrtv"\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})', String.Escape),
            (r'[^\\"\n]+', String), # all other characters
            (r'\\\n', String), # line continuation
            (r'\\', String), # stray backslash
        ],
    }


class ObjectiveCLexer(RegexLexer):
    """
    For Objective-C source code with preprocessor directives.
    """

    name = 'Objective-C'
    aliases = ['objective-c', 'objectivec', 'obj-c', 'objc']
    # XXX: objc has .h files too :-/
    filenames = ['*.m']
    mimetypes = ['text/x-objective-c']

    #: optional Comment or Whitespace
    _ws = r'(?:\s|//.*?\n|/[*].*?[*]/)+'
    #: only one /* */ style comment
    _ws1 = r':\s*/[*].*?[*]/\s*'

    tokens = {
        'whitespace': [
            # preprocessor directives: without whitespace
            ('^#if\s+0', Comment.Preproc, 'if0'),
            ('^#', Comment.Preproc, 'macro'),
            # or with whitespace
            ('^(' + _ws1 + r')(#if\s+0)',
             bygroups(using(this), Comment.Preproc), 'if0'),
            ('^(' + _ws1 + ')(#)',
             bygroups(using(this), Comment.Preproc), 'macro'),
            (r'\n', Text),
            (r'\s+', Text),
            (r'\\\n', Text), # line continuation
            (r'//(\n|(.|\n)*?[^\\]\n)', Comment.Single),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment.Multiline),
        ],
        'statements': [
            (r'(L|@)?"', String, 'string'),
            (r"(L|@)?'(\\.|\\[0-7]{1,3}|\\x[a-fA-F0-9]{1,2}|[^\\\'\n])'",
             String.Char),
            (r'(\d+\.\d*|\.\d+|\d+)[eE][+-]?\d+[lL]?', Number.Float),
            (r'(\d+\.\d*|\.\d+|\d+[fF])[fF]?', Number.Float),
            (r'0x[0-9a-fA-F]+[Ll]?', Number.Hex),
            (r'0[0-7]+[Ll]?', Number.Oct),
            (r'\d+[Ll]?', Number.Integer),
            (r'[~!%^&*+=|?:<>/-]', Operator),
            (r'[()\[\],.]', Punctuation),
            (r'(auto|break|case|const|continue|default|do|else|enum|extern|'
             r'for|goto|if|register|restricted|return|sizeof|static|struct|'
             r'switch|typedef|union|volatile|virtual|while|in|@selector|'
             r'@private|@protected|@public|@encode|'
             r'@synchronized|@try|@throw|@catch|@finally|@end|@property|'
             r'@synthesize|@dynamic|@optional)\b', Keyword),
            (r'(int|long|float|short|double|char|unsigned|signed|void|'
             r'id|BOOL|IBOutlet|IBAction|SEL)\b', Keyword.Type),
            (r'(_{0,2}inline|naked|restrict|thread|typename)\b',
             Keyword.Reserved),
            (r'__(asm|int8|based|except|int16|stdcall|cdecl|fastcall|int32|'
             r'declspec|finally|int64|try|leave)\b', Keyword.Reserved),
            (r'(TRUE|FALSE|nil|NULL)\b', Name.Builtin),
            ('[a-zA-Z$_][a-zA-Z0-9$_]*:(?!:)', Name.Label),
            ('[a-zA-Z$_][a-zA-Z0-9$_]*', Name),
            (r'(@interface|@implementation)(\s+)', bygroups(Keyword, Text),
             ('#pop', 'classname')),
            (r'(@class|@protocol)(\s+)', bygroups(Keyword, Text),
             ('#pop', 'forward_classname')),
        ],
        'root': [
            include('whitespace'),
            # functions
            (r'((?:[a-zA-Z0-9_*\s])+?(?:\s|[*]))'    # return arguments
             r'([a-zA-Z$_][a-zA-Z0-9$_]*)'           # method name
             r'(\s*\([^;]*?\))'                      # signature
             r'(' + _ws + r')?({)',
             bygroups(using(this), Name.Function,
                      using(this), Text, Punctuation),
             'function'),
            # methods
            (r'^([-+])(\s*)'                         # method marker
             r'(\(.*?\))?(\s*)'                      # return type
             r'([a-zA-Z$_][a-zA-Z0-9$_]*:?)',        # begin of method name
             bygroups(Keyword, Text, using(this),
                      Text, Name.Function),
             'method'),
            # function declarations
            (r'((?:[a-zA-Z0-9_*\s])+?(?:\s|[*]))'    # return arguments
             r'([a-zA-Z$_][a-zA-Z0-9$_]*)'           # method name
             r'(\s*\([^;]*?\))'                      # signature
             r'(' + _ws + r')?(;)',
             bygroups(using(this), Name.Function,
                      using(this), Text, Punctuation)),
            (r'(@interface|@implementation)(\s+)', bygroups(Keyword, Text),
             'classname'),
            (r'(@class|@protocol)(\s+)', bygroups(Keyword, Text),
             'forward_classname'),
            (r'(\s*)(@end)(\s*)', bygroups(Text, Keyword, Text)),
            ('', Text, 'statement'),
        ],
        'classname' : [
            # interface definition that inherits
            ('([a-zA-Z$_][a-zA-Z0-9$_]*)(\s*:\s*)([a-zA-Z$_][a-zA-Z0-9$_]*)?',
             bygroups(Name.Class, Text, Name.Class), '#pop'),
            # interface definition for a category
            ('([a-zA-Z$_][a-zA-Z0-9$_]*)(\s*)(\([a-zA-Z$_][a-zA-Z0-9$_]*\))',
             bygroups(Name.Class, Text, Name.Label), '#pop'),
            # simple interface / implementation
            ('([a-zA-Z$_][a-zA-Z0-9$_]*)', Name.Class, '#pop')
        ],
        'forward_classname' : [
          ('([a-zA-Z$_][a-zA-Z0-9$_]*)(\s*,\s*)',
           bygroups(Name.Class, Text), 'forward_classname'),
          ('([a-zA-Z$_][a-zA-Z0-9$_]*)(\s*;?)',
           bygroups(Name.Class, Text), '#pop')
        ],
        'statement' : [
            include('whitespace'),
            include('statements'),
            ('[{}]', Punctuation),
            (';', Punctuation, '#pop'),
        ],
        'function': [
            include('whitespace'),
            include('statements'),
            (';', Punctuation),
            ('{', Punctuation, '#push'),
            ('}', Punctuation, '#pop'),
        ],
        'method': [
            include('whitespace'),
            (r'(\(.*?\))([a-zA-Z$_][a-zA-Z0-9$_]*)', bygroups(using(this),
                                                              Name.Variable)),
            (r'[a-zA-Z$_][a-zA-Z0-9$_]*:', Name.Function),
            (';', Punctuation, '#pop'),
            ('{', Punctuation, 'function'),
            ('', Text, '#pop'),
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'\\([\\abfnrtv"\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})', String.Escape),
            (r'[^\\"\n]+', String), # all other characters
            (r'\\\n', String), # line continuation
            (r'\\', String), # stray backslash
        ],
        'macro': [
            (r'[^/\n]+', Comment.Preproc),
            (r'/[*](.|\n)*?[*]/', Comment.Multiline),
            (r'//.*?\n', Comment.Single, '#pop'),
            (r'/', Comment.Preproc),
            (r'(?<=\\)\n', Comment.Preproc),
            (r'\n', Comment.Preproc, '#pop'),
        ],
        'if0': [
            (r'^\s*#if.*?(?<!\\)\n', Comment.Preproc, '#push'),
            (r'^\s*#endif.*?(?<!\\)\n', Comment.Preproc, '#pop'),
            (r'.*?\n', Comment),
        ]
    }

    def analyse_text(text):
        if '@import' in text or '@interface' in text or \
            '@implementation' in text:
            return True
        elif '@"' in text: # strings
            return True
        elif re.match(r'\[[a-zA-Z0-9.]:', text): # message
            return True
        return False


class FortranLexer(RegexLexer):
    """
    Lexer for FORTRAN 90 code.

    *New in Pygments 0.10.*
    """
    name = 'Fortran'
    aliases = ['fortran']
    filenames = ['*.f', '*.f90', '*.F', '*.F90']
    mimetypes = ['text/x-fortran']
    flags = re.IGNORECASE

    # Data Types: INTEGER, REAL, COMPLEX, LOGICAL, CHARACTER and DOUBLE PRECISION
    # Operators: **, *, +, -, /, <, >, <=, >=, ==, /=
    # Logical (?): NOT, AND, OR, EQV, NEQV

    # Builtins:
    # http://gcc.gnu.org/onlinedocs/gcc-3.4.6/g77/Table-of-Intrinsic-Functions.html

    tokens = {
        'root': [
            (r'!.*\n', Comment),
            include('strings'),
            include('core'),
            (r'[a-z][a-z0-9_]*', Name.Variable),
            include('nums'),
            (r'[\s]+', Text),
        ],
        'core': [
            # Statements
            (r'\b(ACCEPT|ALLOCATABLE|ALLOCATE|ARRAY|ASSIGN|ASYNCHRONOUS|'
             r'BACKSPACE|BIND|BLOCK DATA|BYTE|CALL|CASE|CLOSE|COMMON|CONTAINS|'
             r'CONTINUE|CYCLE|DATA|DEALLOCATE|DECODE|DEFERRED|DIMENSION|DO|'
             r'ELSE|ENCODE|END FILE|ENDIF|END|ENTRY|ENUMERATOR|EQUIVALENCE|'
             r'EXIT|EXTERNAL|EXTRINSIC|FINAL|FORALL|FORMAT|FUNCTION|GENERIC|'
             r'GOTO|IF|IMPLICIT|IMPORT|INCLUDE|INQUIRE|INTENT|INTERFACE|'
             r'INTRINSIC|MODULE|NAMELIST|NULLIFY|NONE|NON_INTRINSIC|'
             r'NON_OVERRIDABLE|NOPASS|OPEN|OPTIONAL|OPTIONS|PARAMETER|PASS|'
             r'PAUSE|POINTER|PRINT|PRIVATE|PROGRAM|PROTECTED|PUBLIC|PURE|READ|'
             r'RECURSIVE|RETURN|REWIND|SAVE|SELECT|SEQUENCE|STOP|SUBROUTINE|'
             r'TARGET|THEN|TYPE|USE|VALUE|VOLATILE|WHERE|WRITE|WHILE)\s*\b',
             Keyword),

            # Data Types
            (r'\b(CHARACTER|COMPLEX|DOUBLE PRECISION|DOUBLE COMPLEX|INTEGER|'
             r'LOGICAL|REAL|C_INT|C_SHORT|C_LONG|C_LONG_LONG|C_SIGNED_CHAR|'
             r'C_SIZE_T|C_INT8_T|C_INT16_T|C_INT32_T|C_INT64_T|C_INT_LEAST8_T|'
             r'C_INT_LEAST16_T|C_INT_LEAST32_T|C_INT_LEAST64_T|C_INT_FAST8_T|'
             r'C_INT_FAST16_T|C_INT_FAST32_T|C_INT_FAST64_T|C_INTMAX_T|'
             r'C_INTPTR_T|C_FLOAT|C_DOUBLE|C_LONG_DOUBLE|C_FLOAT_COMPLEX|'
             r'C_DOUBLE_COMPLEX|C_LONG_DOUBLE_COMPLEX|C_BOOL|C_CHAR|C_PTR|'
             r'C_FUNPTR)\s*\b',
             Keyword.Type),

            # Operators
            (r'(\*\*|\*|\+|-|\/|<|>|<=|>=|==|\/=|=)', Operator),

            (r'(::)', Keyword.Declaration),

            (r'[(),:&%;]', Punctuation),

            # Intrinsics
            (r'\b(Abort|Abs|Access|AChar|ACos|AdjustL|AdjustR|AImag|AInt|Alarm|'
             r'All|Allocated|ALog|AMax|AMin|AMod|And|ANInt|Any|ASin|Associated|'
             r'ATan|BesJ|BesJN|BesY|BesYN|Bit_Size|BTest|CAbs|CCos|Ceiling|'
             r'CExp|Char|ChDir|ChMod|CLog|Cmplx|Command_Argument_Count|Complex|'
             r'Conjg|Cos|CosH|Count|CPU_Time|CShift|CSin|CSqRt|CTime|C_Funloc|'
             r'C_Loc|C_Associated|C_Null_Ptr|C_Null_Funptr|C_F_Pointer|'
             r'C_Null_Char|C_Alert|C_Backspace|C_Form_Feed|C_New_Line|'
             r'C_Carriage_Return|C_Horizontal_Tab|C_Vertical_Tab|'
             r'DAbs|DACos|DASin|DATan|Date_and_Time|DbesJ|'
             r'DbesJ|DbesJN|DbesY|DbesY|DbesYN|Dble|DCos|DCosH|DDiM|DErF|DErFC|'
             r'DExp|Digits|DiM|DInt|DLog|DLog|DMax|DMin|DMod|DNInt|Dot_Product|'
             r'DProd|DSign|DSinH|DSin|DSqRt|DTanH|DTan|DTime|EOShift|Epsilon|'
             r'ErF|ErFC|ETime|Exit|Exp|Exponent|Extends_Type_Of|FDate|FGet|'
             r'FGetC|Float|Floor|Flush|FNum|FPutC|FPut|Fraction|FSeek|FStat|'
             r'FTell|GError|GetArg|Get_Command|Get_Command_Argument|'
             r'Get_Environment_Variable|GetCWD|GetEnv|GetGId|GetLog|GetPId|'
             r'GetUId|GMTime|HostNm|Huge|IAbs|IAChar|IAnd|IArgC|IBClr|IBits|'
             r'IBSet|IChar|IDate|IDiM|IDInt|IDNInt|IEOr|IErrNo|IFix|Imag|'
             r'ImagPart|Index|Int|IOr|IRand|IsaTty|IShft|IShftC|ISign|'
             r'Iso_C_Binding|Is_Iostat_End|Is_Iostat_Eor|ITime|Kill|Kind|'
             r'LBound|Len|Len_Trim|LGe|LGt|Link|LLe|LLt|LnBlnk|Loc|Log|'
             r'Logical|Long|LShift|LStat|LTime|MatMul|Max|MaxExponent|MaxLoc|'
             r'MaxVal|MClock|Merge|Move_Alloc|Min|MinExponent|MinLoc|MinVal|'
             r'Mod|Modulo|MvBits|Nearest|New_Line|NInt|Not|Or|Pack|PError|'
             r'Precision|Present|Product|Radix|Rand|Random_Number|Random_Seed|'
             r'Range|Real|RealPart|Rename|Repeat|Reshape|RRSpacing|RShift|'
             r'Same_Type_As|Scale|Scan|Second|Selected_Int_Kind|'
             r'Selected_Real_Kind|Set_Exponent|Shape|Short|Sign|Signal|SinH|'
             r'Sin|Sleep|Sngl|Spacing|Spread|SqRt|SRand|Stat|Sum|SymLnk|'
             r'System|System_Clock|Tan|TanH|Time|Tiny|Transfer|Transpose|Trim|'
             r'TtyNam|UBound|UMask|Unlink|Unpack|Verify|XOr|ZAbs|ZCos|ZExp|'
             r'ZLog|ZSin|ZSqRt)\s*\b',
             Name.Builtin),

            # Booleans
            (r'\.(true|false)\.', Name.Builtin),
            # Comparing Operators
            (r'\.(eq|ne|lt|le|gt|ge|not|and|or|eqv|neqv)\.', Operator.Word),
        ],

        'strings': [
            (r'(?s)"(\\\\|\\[0-7]+|\\.|[^"\\])*"', String.Double),
            (r"(?s)'(\\\\|\\[0-7]+|\\.|[^'\\])*'", String.Single),
        ],

        'nums': [
            (r'\d+(?![.Ee])', Number.Integer),
            (r'[+-]?\d*\.\d+([eE][-+]?\d+)?', Number.Float),
            (r'[+-]?\d+\.\d*([eE][-+]?\d+)?', Number.Float),
        ],
    }


class GLShaderLexer(RegexLexer):
    """
    GLSL (OpenGL Shader) lexer.

    *New in Pygments 1.1.*
    """
    name = 'GLSL'
    aliases = ['glsl']
    filenames = ['*.vert', '*.frag', '*.geo']
    mimetypes = ['text/x-glslsrc']

    tokens = {
        'root': [
            (r'^#.*', Comment.Preproc),
            (r'//.*', Comment.Single),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment.Multiline),
            (r'\+|-|~|!=?|\*|/|%|<<|>>|<=?|>=?|==?|&&?|\^|\|\|?',
             Operator),
            (r'[?:]', Operator), # quick hack for ternary
            (r'\bdefined\b', Operator),
            (r'[;{}(),\[\]]', Punctuation),
            #FIXME when e is present, no decimal point needed
            (r'[+-]?\d*\.\d+([eE][-+]?\d+)?', Number.Float),
            (r'[+-]?\d+\.\d*([eE][-+]?\d+)?', Number.Float),
            (r'0[xX][0-9a-fA-F]*', Number.Hex),
            (r'0[0-7]*', Number.Oct),
            (r'[1-9][0-9]*', Number.Integer),
            (r'\b(attribute|const|uniform|varying|centroid|break|continue|'
             r'do|for|while|if|else|in|out|inout|float|int|void|bool|true|'
             r'false|invariant|discard|return|mat[234]|mat[234]x[234]|'
             r'vec[234]|[ib]vec[234]|sampler[123]D|samplerCube|'
             r'sampler[12]DShadow|struct)\b', Keyword),
            (r'\b(asm|class|union|enum|typedef|template|this|packed|goto|'
             r'switch|default|inline|noinline|volatile|public|static|extern|'
             r'external|interface|long|short|double|half|fixed|unsigned|'
             r'lowp|mediump|highp|precision|input|output|hvec[234]|'
             r'[df]vec[234]|sampler[23]DRect|sampler2DRectShadow|sizeof|'
             r'cast|namespace|using)\b', Keyword), #future use
            (r'[a-zA-Z_][a-zA-Z_0-9]*', Name),
            (r'\.', Punctuation),
            (r'\s+', Text),
        ],
    }


class PrologLexer(RegexLexer):
    """
    Lexer for Prolog files.
    """
    name = 'Prolog'
    aliases = ['prolog']
    filenames = ['*.prolog', '*.pro', '*.pl']
    mimetypes = ['text/x-prolog']

    flags = re.UNICODE

    tokens = {
        'root': [
            (r'^#.*', Comment.Single),
            (r'/\*', Comment.Multiline, 'nested-comment'),
            (r'%.*', Comment.Single),
            (r'[0-9]+', Number),
            (r'[\[\](){}|.,;!]', Punctuation),
            (r':-|-->', Punctuation),
            (r'"(?:\\x[0-9a-fA-F]+\\|\\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8}|'
             r'\\[0-7]+\\|\\[\w\W]|[^"])*"', String.Double),
            (r"'(?:''|[^'])*'", String.Atom), # quoted atom
            # Needs to not be followed by an atom.
            #(r'=(?=\s|[a-zA-Z\[])', Operator),
            (r'(is|<|>|=<|>=|==|=:=|=|/|//|\*|\+|-)(?=\s|[a-zA-Z0-9\[])',
             Operator),
            (r'(mod|div|not)\b', Operator),
            (r'_', Keyword), # The don't-care variable
            (r'([a-z]+)(:)', bygroups(Name.Namespace, Punctuation)),
            (u'([a-z\u00c0-\u1fff\u3040-\ud7ff\ue000-\uffef]'
             u'[a-zA-Z0-9_$\u00c0-\u1fff\u3040-\ud7ff\ue000-\uffef]*)'
             u'(\\s*)(:-|-->)',
             bygroups(Name.Function, Text, Operator)), # function defn
            (u'([a-z\u00c0-\u1fff\u3040-\ud7ff\ue000-\uffef]'
             u'[a-zA-Z0-9_$\u00c0-\u1fff\u3040-\ud7ff\ue000-\uffef]*)'
             u'(\\s*)(\\()',
             bygroups(Name.Function, Text, Punctuation)),
            (u'[a-z\u00c0-\u1fff\u3040-\ud7ff\ue000-\uffef]'
             u'[a-zA-Z0-9_$\u00c0-\u1fff\u3040-\ud7ff\ue000-\uffef]*',
             String.Atom), # atom, characters
            # This one includes !
            (u'[#&*+\\-./:<=>?@\\\\^~\u00a1-\u00bf\u2010-\u303f]+',
             String.Atom), # atom, graphics
            (r'[A-Z_][A-Za-z0-9_]*', Name.Variable),
            (u'\\s+|[\u2000-\u200f\ufff0-\ufffe\uffef]', Text),
        ],
        'nested-comment': [
            (r'\*/', Comment.Multiline, '#pop'),
            (r'/\*', Comment.Multiline, '#push'),
            (r'[^*/]+', Comment.Multiline),
            (r'[*/]', Comment.Multiline),
        ],
    }

    def analyse_text(text):
        return ':-' in text


class CythonLexer(RegexLexer):
    """
    For Pyrex and `Cython <http://cython.org>`_ source code.

    *New in Pygments 1.1.*
    """

    name = 'Cython'
    aliases = ['cython', 'pyx']
    filenames = ['*.pyx', '*.pxd', '*.pxi']
    mimetypes = ['text/x-cython', 'application/x-cython']

    tokens = {
        'root': [
            (r'\n', Text),
            (r'^(\s*)("""(?:.|\n)*?""")', bygroups(Text, String.Doc)),
            (r"^(\s*)('''(?:.|\n)*?''')", bygroups(Text, String.Doc)),
            (r'[^\S\n]+', Text),
            (r'#.*$', Comment),
            (r'[]{}:(),;[]', Punctuation),
            (r'\\\n', Text),
            (r'\\', Text),
            (r'(in|is|and|or|not)\b', Operator.Word),
            (r'(<)([a-zA-Z0-9.?]+)(>)',
             bygroups(Punctuation, Keyword.Type, Punctuation)),
            (r'!=|==|<<|>>|[-~+/*%=<>&^|.?]', Operator),
            (r'(from)(\d+)(<=)(\s+)(<)(\d+)(:)',
             bygroups(Keyword, Number.Integer, Operator, Name, Operator,
                      Name, Punctuation)),
            include('keywords'),
            (r'(def|property)(\s+)', bygroups(Keyword, Text), 'funcname'),
            (r'(cp?def)(\s+)', bygroups(Keyword, Text), 'cdef'),
            (r'(class|struct)(\s+)', bygroups(Keyword, Text), 'classname'),
            (r'(from)(\s+)', bygroups(Keyword, Text), 'fromimport'),
            (r'(c?import)(\s+)', bygroups(Keyword, Text), 'import'),
            include('builtins'),
            include('backtick'),
            ('(?:[rR]|[uU][rR]|[rR][uU])"""', String, 'tdqs'),
            ("(?:[rR]|[uU][rR]|[rR][uU])'''", String, 'tsqs'),
            ('(?:[rR]|[uU][rR]|[rR][uU])"', String, 'dqs'),
            ("(?:[rR]|[uU][rR]|[rR][uU])'", String, 'sqs'),
            ('[uU]?"""', String, combined('stringescape', 'tdqs')),
            ("[uU]?'''", String, combined('stringescape', 'tsqs')),
            ('[uU]?"', String, combined('stringescape', 'dqs')),
            ("[uU]?'", String, combined('stringescape', 'sqs')),
            include('name'),
            include('numbers'),
        ],
        'keywords': [
            (r'(assert|break|by|continue|ctypedef|del|elif|else|except\??|exec|'
             r'finally|for|gil|global|if|include|lambda|nogil|pass|print|raise|'
             r'return|try|while|yield|as|with)\b', Keyword),
            (r'(DEF|IF|ELIF|ELSE)\b', Comment.Preproc),
        ],
        'builtins': [
            (r'(?<!\.)(__import__|abs|all|any|apply|basestring|bin|bool|buffer|'
             r'bytearray|bytes|callable|chr|classmethod|cmp|coerce|compile|'
             r'complex|delattr|dict|dir|divmod|enumerate|eval|execfile|exit|'
             r'file|filter|float|frozenset|getattr|globals|hasattr|hash|hex|id|'
             r'input|int|intern|isinstance|issubclass|iter|len|list|locals|'
             r'long|map|max|min|next|object|oct|open|ord|pow|property|range|'
             r'raw_input|reduce|reload|repr|reversed|round|set|setattr|slice|'
             r'sorted|staticmethod|str|sum|super|tuple|type|unichr|unicode|'
             r'vars|xrange|zip)\b', Name.Builtin),
            (r'(?<!\.)(self|None|Ellipsis|NotImplemented|False|True|NULL'
             r')\b', Name.Builtin.Pseudo),
            (r'(?<!\.)(ArithmeticError|AssertionError|AttributeError|'
             r'BaseException|DeprecationWarning|EOFError|EnvironmentError|'
             r'Exception|FloatingPointError|FutureWarning|GeneratorExit|IOError|'
             r'ImportError|ImportWarning|IndentationError|IndexError|KeyError|'
             r'KeyboardInterrupt|LookupError|MemoryError|NameError|'
             r'NotImplemented|NotImplementedError|OSError|OverflowError|'
             r'OverflowWarning|PendingDeprecationWarning|ReferenceError|'
             r'RuntimeError|RuntimeWarning|StandardError|StopIteration|'
             r'SyntaxError|SyntaxWarning|SystemError|SystemExit|TabError|'
             r'TypeError|UnboundLocalError|UnicodeDecodeError|'
             r'UnicodeEncodeError|UnicodeError|UnicodeTranslateError|'
             r'UnicodeWarning|UserWarning|ValueError|Warning|ZeroDivisionError'
             r')\b', Name.Exception),
        ],
        'numbers': [
            (r'(\d+\.?\d*|\d*\.\d+)([eE][+-]?[0-9]+)?', Number.Float),
            (r'0\d+', Number.Oct),
            (r'0[xX][a-fA-F0-9]+', Number.Hex),
            (r'\d+L', Number.Integer.Long),
            (r'\d+', Number.Integer)
        ],
        'backtick': [
            ('`.*?`', String.Backtick),
        ],
        'name': [
            (r'@[a-zA-Z0-9_]+', Name.Decorator),
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name),
        ],
        'funcname': [
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name.Function, '#pop')
        ],
        'cdef': [
            (r'(public|readonly|extern|api|inline)\b', Keyword.Reserved),
            (r'(struct|enum|union|class)\b', Keyword),
            (r'([a-zA-Z_][a-zA-Z0-9_]*)(\s*)(?=[(:#=]|$)',
             bygroups(Name.Function, Text), '#pop'),
            (r'([a-zA-Z_][a-zA-Z0-9_]*)(\s*)(,)',
             bygroups(Name.Function, Text, Punctuation)),
            (r'from\b', Keyword, '#pop'),
            (r'as\b', Keyword),
            (r':', Punctuation, '#pop'),
            (r'(?=["\'])', Text, '#pop'),
            (r'[a-zA-Z_][a-zA-Z0-9_]*', Keyword.Type),
            (r'.', Text),
        ],
        'classname': [
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name.Class, '#pop')
        ],
        'import': [
            (r'(\s+)(as)(\s+)', bygroups(Text, Keyword, Text)),
            (r'[a-zA-Z_][a-zA-Z0-9_.]*', Name.Namespace),
            (r'(\s*)(,)(\s*)', bygroups(Text, Operator, Text)),
            (r'', Text, '#pop') # all else: go back
        ],
        'fromimport': [
            (r'(\s+)(c?import)\b', bygroups(Text, Keyword), '#pop'),
            (r'[a-zA-Z_.][a-zA-Z0-9_.]*', Name.Namespace),
            # ``cdef foo from "header"``, or ``for foo from 0 < i < 10``
            (r'', Text, '#pop'),
        ],
        'stringescape': [
            (r'\\([\\abfnrtv"\']|\n|N{.*?}|u[a-fA-F0-9]{4}|'
             r'U[a-fA-F0-9]{8}|x[a-fA-F0-9]{2}|[0-7]{1,3})', String.Escape)
        ],
        'strings': [
            (r'%(\([a-zA-Z0-9]+\))?[-#0 +]*([0-9]+|[*])?(\.([0-9]+|[*]))?'
             '[hlL]?[diouxXeEfFgGcrs%]', String.Interpol),
            (r'[^\\\'"%\n]+', String),
            # quotes, percents and backslashes must be parsed one at a time
            (r'[\'"\\]', String),
            # unhandled string formatting sign
            (r'%', String)
            # newlines are an error (use "nl" state)
        ],
        'nl': [
            (r'\n', String)
        ],
        'dqs': [
            (r'"', String, '#pop'),
            (r'\\\\|\\"|\\\n', String.Escape), # included here again for raw strings
            include('strings')
        ],
        'sqs': [
            (r"'", String, '#pop'),
            (r"\\\\|\\'|\\\n", String.Escape), # included here again for raw strings
            include('strings')
        ],
        'tdqs': [
            (r'"""', String, '#pop'),
            include('strings'),
            include('nl')
        ],
        'tsqs': [
            (r"'''", String, '#pop'),
            include('strings'),
            include('nl')
        ],
    }


class ValaLexer(RegexLexer):
    """
    For Vala source code with preprocessor directives.

    *New in Pygments 1.1.*
    """
    name = 'Vala'
    aliases = ['vala', 'vapi']
    filenames = ['*.vala', '*.vapi']
    mimetypes = ['text/x-vala']

    tokens = {
        'whitespace': [
            (r'^\s*#if\s+0', Comment.Preproc, 'if0'),
            (r'\n', Text),
            (r'\s+', Text),
            (r'\\\n', Text), # line continuation
            (r'//(\n|(.|\n)*?[^\\]\n)', Comment.Single),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment.Multiline),
        ],
        'statements': [
            (r'L?"', String, 'string'),
            (r"L?'(\\.|\\[0-7]{1,3}|\\x[a-fA-F0-9]{1,2}|[^\\\'\n])'",
             String.Char),
            (r'(\d+\.\d*|\.\d+|\d+)[eE][+-]?\d+[lL]?', Number.Float),
            (r'(\d+\.\d*|\.\d+|\d+[fF])[fF]?', Number.Float),
            (r'0x[0-9a-fA-F]+[Ll]?', Number.Hex),
            (r'0[0-7]+[Ll]?', Number.Oct),
            (r'\d+[Ll]?', Number.Integer),
            (r'[~!%^&*+=|?:<>/-]', Operator),
            (r'(\[)(Compact|Immutable|(?:Boolean|Simple)Type)(\])',
             bygroups(Punctuation, Name.Decorator, Punctuation)),
            # TODO: "correctly" parse complex code attributes
            (r'(\[)(CCode|(?:Integer|Floating)Type)',
             bygroups(Punctuation, Name.Decorator)),
            (r'[()\[\],.]', Punctuation),
            (r'(as|base|break|case|catch|construct|continue|default|delete|do|'
             r'else|enum|finally|for|foreach|get|if|in|is|lock|new|out|params|'
             r'return|set|sizeof|switch|this|throw|try|typeof|while|yield)\b',
             Keyword),
            (r'(abstract|const|delegate|dynamic|ensures|extern|inline|internal|'
             r'override|owned|private|protected|public|ref|requires|signal|'
             r'static|throws|unowned|var|virtual|volatile|weak|yields)\b',
             Keyword.Declaration),
            (r'(namespace|using)(\s+)', bygroups(Keyword.Namespace, Text),
             'namespace'),
            (r'(class|errordomain|interface|struct)(\s+)',
             bygroups(Keyword.Declaration, Text), 'class'),
            (r'(\.)([a-zA-Z_][a-zA-Z0-9_]*)',
             bygroups(Operator, Name.Attribute)),
            # void is an actual keyword, others are in glib-2.0.vapi
            (r'(void|bool|char|double|float|int|int8|int16|int32|int64|long|'
             r'short|size_t|ssize_t|string|time_t|uchar|uint|uint8|uint16|'
             r'uint32|uint64|ulong|unichar|ushort)\b', Keyword.Type),
            (r'(true|false|null)\b', Name.Builtin),
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name),
        ],
        'root': [
            include('whitespace'),
            ('', Text, 'statement'),
        ],
        'statement' : [
            include('whitespace'),
            include('statements'),
            ('[{}]', Punctuation),
            (';', Punctuation, '#pop'),
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'\\([\\abfnrtv"\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})', String.Escape),
            (r'[^\\"\n]+', String), # all other characters
            (r'\\\n', String), # line continuation
            (r'\\', String), # stray backslash
        ],
        'if0': [
            (r'^\s*#if.*?(?<!\\)\n', Comment.Preproc, '#push'),
            (r'^\s*#el(?:se|if).*\n', Comment.Preproc, '#pop'),
            (r'^\s*#endif.*?(?<!\\)\n', Comment.Preproc, '#pop'),
            (r'.*?\n', Comment),
        ],
        'class': [
            (r'[a-zA-Z_][a-zA-Z0-9_]*', Name.Class, '#pop')
        ],
        'namespace': [
            (r'[a-zA-Z_][a-zA-Z0-9_.]*', Name.Namespace, '#pop')
        ],
    }


class OocLexer(RegexLexer):
    """
    For `Ooc <http://ooc-lang.org/>`_ source code

    *New in Pygments 1.2.*
    """
    name = 'Ooc'
    aliases = ['ooc']
    filenames = ['*.ooc']
    mimetypes = ['text/x-ooc']

    tokens = {
        'root': [
            (r'\b(class|interface|implement|abstract|extends|from|'
             r'this|super|new|const|final|static|import|use|extern|'
             r'inline|proto|break|continue|fallthrough|operator|if|else|for|'
             r'while|do|switch|case|as|in|version|return|true|false|null)\b',
             Keyword),
            (r'include\b', Keyword, 'include'),
            (r'(cover)([ \t]+)(from)([ \t]+)([a-zA-Z0-9_]+[*@]?)',
             bygroups(Keyword, Text, Keyword, Text, Name.Class)),
            (r'(func)((?:[ \t]|\\\n)+)(~[a-z_][a-zA-Z0-9_]*)',
             bygroups(Keyword, Text, Name.Function)),
            (r'\bfunc\b', Keyword),
            # Note: %= and ^= not listed on http://ooc-lang.org/syntax
            (r'//.*', Comment),
            (r'(?s)/\*.*?\*/', Comment.Multiline),
            (r'(==?|\+=?|-[=>]?|\*=?|/=?|:=|!=?|%=?|\?|>{1,3}=?|<{1,3}=?|\.\.|'
             r'&&?|\|\|?|\^=?)', Operator),
            (r'(\.)([ \t]*)([a-z]\w*)', bygroups(Operator, Text,
                                                 Name.Function)),
            (r'[A-Z][A-Z0-9_]+', Name.Constant),
            (r'[A-Z][a-zA-Z0-9_]*([@*]|\[[ \t]*\])?', Name.Class),

            (r'([a-z][a-zA-Z0-9_]*(?:~[a-z][a-zA-Z0-9_]*)?)((?:[ \t]|\\\n)*)(?=\()',
             bygroups(Name.Function, Text)),
            (r'[a-z][a-zA-Z0-9_]*', Name.Variable),

            # : introduces types
            (r'[:(){}\[\];,]', Punctuation),

            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'0c[0-9]+', Number.Oct),
            (r'0b[01]+', Number.Binary),
            (r'[0-9_]\.[0-9_]*(?!\.)', Number.Float),
            (r'[0-9_]+', Number.Decimal),

            (r'"(?:\\.|\\[0-7]{1,3}|\\x[a-fA-F0-9]{1,2}|[^\\\"])*"',
             String.Double),
            (r"'(?:\\.|\\[0-9]{1,3}|\\x[a-fA-F0-9]{1,2}|[^\\\'\n])'",
             String.Char),
            (r'@', Punctuation), # pointer dereference
            (r'\.', Punctuation), # imports or chain operator

            (r'\\[ \t\n]', Text),
            (r'[ \t]+', Text),
        ],
        'include': [
            (r'[\w/]+', Name),
            (r',', Punctuation),
            (r'[ \t]', Text),
            (r'[;\n]', Text, '#pop'),
        ],
    }


class GoLexer(RegexLexer):
    """
    For `Go <http://golang.org>`_ source.
    """
    name = 'Go'
    filenames = ['*.go']
    aliases = ['go']
    mimetypes = ['text/x-gosrc']

    tokens = {
        'root': [
            (r'\n', Text),
            (r'\s+', Text),
            (r'\\\n', Text), # line continuations
            (r'//(.*?)\n', Comment.Single),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment.Multiline),
            (r'(import|package)\b', Keyword.Namespace),
            (r'(var|func|struct|map|chan|type|interface|const)\b', Keyword.Declaration),
            (r'(break|default|select|case|defer|go'
             r'|else|goto|switch|fallthrough|if|range'
             r'|continue|for|return)\b', Keyword
            ),
            (r'(true|false|iota|nil)\b', Keyword.Constant),
            # It seems the builtin types aren't actually keywords, but
            # can be used as functions. So we need two declarations.
            (r'(uint|uint8|uint16|uint32|uint64'
             r'|int|int8|int16|int32|int64'
             r'|float|float32|float64'
             r'|complex64|complex128|byte|rune'
             r'|string|bool|error|uintptr'
             r'|print|println|panic|recover|close|complex|real|imag'
             r'|len|cap|append|copy|delete|new|make)\b(\()', bygroups(Name.Builtin, Punctuation)
            ),
            (r'(uint|uint8|uint16|uint32|uint64'
             r'|int|int8|int16|int32|int64'
             r'|float|float32|float64'
             r'|complex64|complex128|byte|rune'
             r'|string|bool|error|uintptr)\b', Keyword.Type
            ),
            # imaginary_lit
            (r'\d+i', Number),
            (r'\d+\.\d*([Ee][-+]\d+)?i', Number),
            (r'\.\d+([Ee][-+]\d+)?i', Number),
            (r'\d+[Ee][-+]\d+i', Number),
            # float_lit
            (r'\d+(\.\d+[eE][+\-]?\d+|'
             r'\.\d*|[eE][+\-]?\d+)', Number.Float),
            (r'\.\d+([eE][+\-]?\d+)?', Number.Float),
            # int_lit
            # -- octal_lit
            (r'0[0-7]+', Number.Oct),
            # -- hex_lit
            (r'0[xX][0-9a-fA-F]+', Number.Hex),
            # -- decimal_lit
            (r'(0|[1-9][0-9]*)', Number.Integer),
            # char_lit
            (r"""'(\\['"\\abfnrtv]|\\x[0-9a-fA-F]{2}|\\[0-7]{1,3}"""
             r"""|\\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8}|[^\\])'""",
             String.Char
            ),
            # StringLiteral
            # -- raw_string_lit
            (r'`[^`]*`', String),
            # -- interpreted_string_lit
            (r'"(\\\\|\\"|[^"])*"', String),
            # Tokens
            (r'(<<=|>>=|<<|>>|<=|>=|&\^=|&\^|\+=|-=|\*=|/=|%=|&=|\|=|&&|\|\|'
             r'|<-|\+\+|--|==|!=|:=|\.\.\.|[+\-*/%&])', Operator),
            (r'[|^<>=!()\[\]{}.,;:]', Punctuation),
            # identifier
            (r'[a-zA-Z_]\w*', Name.Other),
        ]
    }


class FelixLexer(RegexLexer):
    """
    For `Felix <http://www.felix-lang.org>`_ source code.

    *New in Pygments 1.2.*
    """

    name = 'Felix'
    aliases = ['felix', 'flx']
    filenames = ['*.flx', '*.flxh']
    mimetypes = ['text/x-felix']

    preproc = [
        'elif', 'else', 'endif', 'if', 'ifdef', 'ifndef',
    ]

    keywords = [
        '_', '_deref', 'all', 'as',
        'assert', 'attempt', 'call', 'callback', 'case', 'caseno', 'cclass',
        'code', 'compound', 'ctypes', 'do', 'done', 'downto', 'elif', 'else',
        'endattempt', 'endcase', 'endif', 'endmatch', 'enum', 'except',
        'exceptions', 'expect', 'finally', 'for', 'forall', 'forget', 'fork',
        'functor', 'goto', 'ident', 'if', 'incomplete', 'inherit', 'instance',
        'interface', 'jump', 'lambda', 'loop', 'match', 'module', 'namespace',
        'new', 'noexpand', 'nonterm', 'obj', 'of', 'open', 'parse', 'raise',
        'regexp', 'reglex', 'regmatch', 'rename', 'return', 'the', 'then',
        'to', 'type', 'typecase', 'typedef', 'typematch', 'typeof', 'upto',
        'when', 'whilst', 'with', 'yield',
    ]

    keyword_directives = [
        '_gc_pointer', '_gc_type', 'body', 'comment', 'const', 'export',
        'header', 'inline', 'lval', 'macro', 'noinline', 'noreturn',
        'package', 'private', 'pod', 'property', 'public', 'publish',
        'requires', 'todo', 'virtual', 'use',
    ]

    keyword_declarations = [
        'def', 'let', 'ref', 'val', 'var',
    ]

    keyword_types = [
        'unit', 'void', 'any', 'bool',
        'byte',  'offset',
        'address', 'caddress', 'cvaddress', 'vaddress',
        'tiny', 'short', 'int', 'long', 'vlong',
        'utiny', 'ushort', 'vshort', 'uint', 'ulong', 'uvlong',
        'int8', 'int16', 'int32', 'int64',
        'uint8', 'uint16', 'uint32', 'uint64',
        'float', 'double', 'ldouble',
        'complex', 'dcomplex', 'lcomplex',
        'imaginary', 'dimaginary', 'limaginary',
        'char', 'wchar', 'uchar',
        'charp', 'charcp', 'ucharp', 'ucharcp',
        'string', 'wstring', 'ustring',
        'cont',
        'array', 'varray', 'list',
        'lvalue', 'opt', 'slice',
    ]

    keyword_constants = [
        'false', 'true',
    ]

    operator_words = [
        'and', 'not', 'in', 'is', 'isin', 'or', 'xor',
    ]

    name_builtins = [
        '_svc', 'while',
    ]

    name_pseudo = [
        'root', 'self', 'this',
    ]

    decimal_suffixes = '([tTsSiIlLvV]|ll|LL|([iIuU])(8|16|32|64))?'

    tokens = {
        'root': [
            include('whitespace'),

            # Keywords
            (r'(axiom|ctor|fun|gen|proc|reduce|union)\b', Keyword,
             'funcname'),
            (r'(class|cclass|cstruct|obj|struct)\b', Keyword, 'classname'),
            (r'(instance|module|typeclass)\b', Keyword, 'modulename'),

            (r'(%s)\b' % '|'.join(keywords), Keyword),
            (r'(%s)\b' % '|'.join(keyword_directives), Name.Decorator),
            (r'(%s)\b' % '|'.join(keyword_declarations), Keyword.Declaration),
            (r'(%s)\b' % '|'.join(keyword_types), Keyword.Type),
            (r'(%s)\b' % '|'.join(keyword_constants), Keyword.Constant),

            # Operators
            include('operators'),

            # Float Literal
            # -- Hex Float
            (r'0[xX]([0-9a-fA-F_]*\.[0-9a-fA-F_]+|[0-9a-fA-F_]+)'
             r'[pP][+\-]?[0-9_]+[lLfFdD]?', Number.Float),
            # -- DecimalFloat
            (r'[0-9_]+(\.[0-9_]+[eE][+\-]?[0-9_]+|'
             r'\.[0-9_]*|[eE][+\-]?[0-9_]+)[lLfFdD]?', Number.Float),
            (r'\.(0|[1-9][0-9_]*)([eE][+\-]?[0-9_]+)?[lLfFdD]?',
             Number.Float),

            # IntegerLiteral
            # -- Binary
            (r'0[Bb][01_]+%s' % decimal_suffixes, Number),
            # -- Octal
            (r'0[0-7_]+%s' % decimal_suffixes, Number.Oct),
            # -- Hexadecimal
            (r'0[xX][0-9a-fA-F_]+%s' % decimal_suffixes, Number.Hex),
            # -- Decimal
            (r'(0|[1-9][0-9_]*)%s' % decimal_suffixes, Number.Integer),

            # Strings
            ('([rR][cC]?|[cC][rR])"""', String, 'tdqs'),
            ("([rR][cC]?|[cC][rR])'''", String, 'tsqs'),
            ('([rR][cC]?|[cC][rR])"', String, 'dqs'),
            ("([rR][cC]?|[cC][rR])'", String, 'sqs'),
            ('[cCfFqQwWuU]?"""', String, combined('stringescape', 'tdqs')),
            ("[cCfFqQwWuU]?'''", String, combined('stringescape', 'tsqs')),
            ('[cCfFqQwWuU]?"', String, combined('stringescape', 'dqs')),
            ("[cCfFqQwWuU]?'", String, combined('stringescape', 'sqs')),

            # Punctuation
            (r'[\[\]{}:(),;?]', Punctuation),

            # Labels
            (r'[a-zA-Z_]\w*:>', Name.Label),

            # Identifiers
            (r'(%s)\b' % '|'.join(name_builtins), Name.Builtin),
            (r'(%s)\b' % '|'.join(name_pseudo), Name.Builtin.Pseudo),
            (r'[a-zA-Z_]\w*', Name),
        ],
        'whitespace': [
            (r'\n', Text),
            (r'\s+', Text),

            include('comment'),

            # Preprocessor
            (r'#\s*if\s+0', Comment.Preproc, 'if0'),
            (r'#', Comment.Preproc, 'macro'),
        ],
        'operators': [
            (r'(%s)\b' % '|'.join(operator_words), Operator.Word),
            (r'!=|==|<<|>>|\|\||&&|[-~+/*%=<>&^|.$]', Operator),
        ],
        'comment': [
            (r'//(.*?)\n', Comment.Single),
            (r'/[*]', Comment.Multiline, 'comment2'),
        ],
        'comment2': [
            (r'[^\/*]', Comment.Multiline),
            (r'/[*]', Comment.Multiline, '#push'),
            (r'[*]/', Comment.Multiline, '#pop'),
            (r'[\/*]', Comment.Multiline),
        ],
        'if0': [
            (r'^\s*#if.*?(?<!\\)\n', Comment, '#push'),
            (r'^\s*#endif.*?(?<!\\)\n', Comment, '#pop'),
            (r'.*?\n', Comment),
        ],
        'macro': [
            include('comment'),
            (r'(import|include)(\s+)(<[^>]*?>)',
             bygroups(Comment.Preproc, Text, String), '#pop'),
            (r'(import|include)(\s+)("[^"]*?")',
             bygroups(Comment.Preproc, Text, String), '#pop'),
            (r"(import|include)(\s+)('[^']*?')",
             bygroups(Comment.Preproc, Text, String), '#pop'),
            (r'[^/\n]+', Comment.Preproc),
            ##(r'/[*](.|\n)*?[*]/', Comment),
            ##(r'//.*?\n', Comment, '#pop'),
            (r'/', Comment.Preproc),
            (r'(?<=\\)\n', Comment.Preproc),
            (r'\n', Comment.Preproc, '#pop'),
        ],
        'funcname': [
            include('whitespace'),
            (r'[a-zA-Z_]\w*', Name.Function, '#pop'),
            # anonymous functions
            (r'(?=\()', Text, '#pop'),
        ],
        'classname': [
            include('whitespace'),
            (r'[a-zA-Z_]\w*', Name.Class, '#pop'),
            # anonymous classes
            (r'(?=\{)', Text, '#pop'),
        ],
        'modulename': [
            include('whitespace'),
            (r'\[', Punctuation, ('modulename2', 'tvarlist')),
            (r'', Error, 'modulename2'),
        ],
        'modulename2': [
            include('whitespace'),
            (r'([a-zA-Z_]\w*)', Name.Namespace, '#pop:2'),
        ],
        'tvarlist': [
            include('whitespace'),
            include('operators'),
            (r'\[', Punctuation, '#push'),
            (r'\]', Punctuation, '#pop'),
            (r',', Punctuation),
            (r'(with|where)\b', Keyword),
            (r'[a-zA-Z_]\w*', Name),
        ],
        'stringescape': [
            (r'\\([\\abfnrtv"\']|\n|N{.*?}|u[a-fA-F0-9]{4}|'
             r'U[a-fA-F0-9]{8}|x[a-fA-F0-9]{2}|[0-7]{1,3})', String.Escape)
        ],
        'strings': [
            (r'%(\([a-zA-Z0-9]+\))?[-#0 +]*([0-9]+|[*])?(\.([0-9]+|[*]))?'
             '[hlL]?[diouxXeEfFgGcrs%]', String.Interpol),
            (r'[^\\\'"%\n]+', String),
            # quotes, percents and backslashes must be parsed one at a time
            (r'[\'"\\]', String),
            # unhandled string formatting sign
            (r'%', String)
            # newlines are an error (use "nl" state)
        ],
        'nl': [
            (r'\n', String)
        ],
        'dqs': [
            (r'"', String, '#pop'),
            # included here again for raw strings
            (r'\\\\|\\"|\\\n', String.Escape),
            include('strings')
        ],
        'sqs': [
            (r"'", String, '#pop'),
            # included here again for raw strings
            (r"\\\\|\\'|\\\n", String.Escape),
            include('strings')
        ],
        'tdqs': [
            (r'"""', String, '#pop'),
            include('strings'),
            include('nl')
        ],
        'tsqs': [
            (r"'''", String, '#pop'),
            include('strings'),
            include('nl')
        ],
     }


class AdaLexer(RegexLexer):
    """
    For Ada source code.

    *New in Pygments 1.3.*
    """

    name = 'Ada'
    aliases = ['ada', 'ada95' 'ada2005']
    filenames = ['*.adb', '*.ads', '*.ada']
    mimetypes = ['text/x-ada']

    flags = re.MULTILINE | re.I  # Ignore case

    tokens = {
        'root': [
            (r'[^\S\n]+', Text),
            (r'--.*?\n', Comment.Single),
            (r'[^\S\n]+', Text),
            (r'function|procedure|entry', Keyword.Declaration, 'subprogram'),
            (r'(subtype|type)(\s+)([a-z0-9_]+)',
             bygroups(Keyword.Declaration, Text, Keyword.Type), 'type_def'),
            (r'task|protected', Keyword.Declaration),
            (r'(subtype)(\s+)', bygroups(Keyword.Declaration, Text)),
            (r'(end)(\s+)', bygroups(Keyword.Reserved, Text), 'end'),
            (r'(pragma)(\s+)([a-zA-Z0-9_]+)', bygroups(Keyword.Reserved, Text,
                                                       Comment.Preproc)),
            (r'(true|false|null)\b', Keyword.Constant),
            (r'(Byte|Character|Float|Integer|Long_Float|Long_Integer|'
             r'Long_Long_Float|Long_Long_Integer|Natural|Positive|Short_Float|'
             r'Short_Integer|Short_Short_Float|Short_Short_Integer|String|'
             r'Wide_String|Duration)\b', Keyword.Type),
            (r'(and(\s+then)?|in|mod|not|or(\s+else)|rem)\b', Operator.Word),
            (r'generic|private', Keyword.Declaration),
            (r'package', Keyword.Declaration, 'package'),
            (r'array\b', Keyword.Reserved, 'array_def'),
            (r'(with|use)(\s+)', bygroups(Keyword.Namespace, Text), 'import'),
            (r'([a-z0-9_]+)(\s*)(:)(\s*)(constant)',
             bygroups(Name.Constant, Text, Punctuation, Text,
                      Keyword.Reserved)),
            (r'<<[a-z0-9_]+>>', Name.Label),
            (r'([a-z0-9_]+)(\s*)(:)(\s*)(declare|begin|loop|for|while)',
             bygroups(Name.Label, Text, Punctuation, Text, Keyword.Reserved)),
            (r'\b(abort|abs|abstract|accept|access|aliased|all|array|at|begin|'
             r'body|case|constant|declare|delay|delta|digits|do|else|elsif|end|'
             r'entry|exception|exit|interface|for|goto|if|is|limited|loop|new|'
             r'null|of|or|others|out|overriding|pragma|protected|raise|range|'
             r'record|renames|requeue|return|reverse|select|separate|subtype|'
             r'synchronized|task|tagged|terminate|then|type|until|when|while|'
             r'xor)\b',
             Keyword.Reserved),
            (r'"[^"]*"', String),
            include('attribute'),
            include('numbers'),
            (r"'[^']'", String.Character),
            (r'([a-z0-9_]+)(\s*|[(,])', bygroups(Name, using(this))),
            (r"(<>|=>|:=|[()|:;,.'])", Punctuation),
            (r'[*<>+=/&-]', Operator),
            (r'\n+', Text),
        ],
        'numbers' : [
            (r'[0-9_]+#[0-9a-f]+#', Number.Hex),
            (r'[0-9_]+\.[0-9_]*', Number.Float),
            (r'[0-9_]+', Number.Integer),
        ],
        'attribute' : [
            (r"(')([a-zA-Z0-9_]+)", bygroups(Punctuation, Name.Attribute)),
        ],
        'subprogram' : [
            (r'\(', Punctuation, ('#pop', 'formal_part')),
            (r';', Punctuation, '#pop'),
            (r'is\b', Keyword.Reserved, '#pop'),
            (r'"[^"]+"|[a-z0-9_]+', Name.Function),
            include('root'),
        ],
        'end' : [
            ('(if|case|record|loop|select)', Keyword.Reserved),
            ('"[^"]+"|[a-zA-Z0-9_.]+', Name.Function),
            ('\s+', Text),
            (';', Punctuation, '#pop'),
        ],
        'type_def': [
            (r';', Punctuation, '#pop'),
            (r'\(', Punctuation, 'formal_part'),
            (r'with|and|use', Keyword.Reserved),
            (r'array\b', Keyword.Reserved, ('#pop', 'array_def')),
            (r'record\b', Keyword.Reserved, ('formal_part')),
            include('root'),
        ],
        'array_def' : [
            (r';', Punctuation, '#pop'),
            (r'([a-z0-9_]+)(\s+)(range)', bygroups(Keyword.Type, Text,
                                                   Keyword.Reserved)),
            include('root'),
        ],
        'import': [
            (r'[a-z0-9_.]+', Name.Namespace, '#pop'),
            (r'', Text, '#pop'),
        ],
        'formal_part' : [
            (r'\)', Punctuation, '#pop'),
            (r'[a-z0-9_]+', Name.Variable),
            (r',|:[^=]', Punctuation),
            (r'(in|not|null|out|access)\b', Keyword.Reserved),
            include('root'),
        ],
        'package': [
            ('body', Keyword.Declaration),
            ('is\s+new|renames', Keyword.Reserved),
            ('is', Keyword.Reserved, '#pop'),
            (';', Punctuation, '#pop'),
            ('\(', Punctuation, 'package_instantiation'),
            ('([a-zA-Z0-9_.]+)', Name.Class),
            include('root'),
        ],
        'package_instantiation': [
            (r'("[^"]+"|[a-z0-9_]+)(\s+)(=>)', bygroups(Name.Variable,
                                                        Text, Punctuation)),
            (r'[a-z0-9._\'"]', Text),
            (r'\)', Punctuation, '#pop'),
            include('root'),
        ],
    }


class Modula2Lexer(RegexLexer):
    """
    For `Modula-2 <http://www.modula2.org/>`_ source code.

    Additional options that determine which keywords are highlighted:

    `pim`
        Select PIM Modula-2 dialect (default: True).
    `iso`
        Select ISO Modula-2 dialect (default: False).
    `objm2`
        Select Objective Modula-2 dialect (default: False).
    `gm2ext`
        Also highlight GNU extensions (default: False).

    *New in Pygments 1.3.*
    """
    name = 'Modula-2'
    aliases = ['modula2', 'm2']
    filenames = ['*.def', '*.mod']
    mimetypes = ['text/x-modula2']

    flags = re.MULTILINE | re.DOTALL

    tokens = {
        'whitespace': [
            (r'\n+', Text), # blank lines
            (r'\s+', Text), # whitespace
        ],
        'identifiers': [
            (r'([a-zA-Z_\$][a-zA-Z0-9_\$]*)', Name),
        ],
        'numliterals': [
            (r'[01]+B', Number.Binary),        # binary number (ObjM2)
            (r'[0-7]+B', Number.Oct),          # octal number (PIM + ISO)
            (r'[0-7]+C', Number.Oct),          # char code (PIM + ISO)
            (r'[0-9A-F]+C', Number.Hex),       # char code (ObjM2)
            (r'[0-9A-F]+H', Number.Hex),       # hexadecimal number
            (r'[0-9]+\.[0-9]+E[+-][0-9]+', Number.Float), # real number
            (r'[0-9]+\.[0-9]+', Number.Float), # real number
            (r'[0-9]+', Number.Integer),       # decimal whole number
        ],
        'strings': [
            (r"'(\\\\|\\'|[^'])*'", String), # single quoted string
            (r'"(\\\\|\\"|[^"])*"', String), # double quoted string
        ],
        'operators': [
            (r'[*/+=#~&<>\^-]', Operator),
            (r':=', Operator),   # assignment
            (r'@', Operator),    # pointer deref (ISO)
            (r'\.\.', Operator), # ellipsis or range
            (r'`', Operator),    # Smalltalk message (ObjM2)
            (r'::', Operator),   # type conversion (ObjM2)
        ],
        'punctuation': [
            (r'[\(\)\[\]{},.:;|]', Punctuation),
        ],
        'comments': [
            (r'//.*?\n', Comment.Single),       # ObjM2
            (r'/\*(.*?)\*/', Comment.Multiline), # ObjM2
            (r'\(\*([^\$].*?)\*\)', Comment.Multiline),
            # TO DO: nesting of (* ... *) comments
        ],
        'pragmas': [
            (r'\(\*\$(.*?)\*\)', Comment.Preproc), # PIM
            (r'<\*(.*?)\*>', Comment.Preproc),     # ISO + ObjM2
        ],
        'root': [
            include('whitespace'),
            include('comments'),
            include('pragmas'),
            include('identifiers'),
            include('numliterals'),
            include('strings'),
            include('operators'),
            include('punctuation'),
        ]
    }

    pim_reserved_words = [
        # 40 reserved words
        'AND', 'ARRAY', 'BEGIN', 'BY', 'CASE', 'CONST', 'DEFINITION',
        'DIV', 'DO', 'ELSE', 'ELSIF', 'END', 'EXIT', 'EXPORT', 'FOR',
        'FROM', 'IF', 'IMPLEMENTATION', 'IMPORT', 'IN', 'LOOP', 'MOD',
        'MODULE', 'NOT', 'OF', 'OR', 'POINTER', 'PROCEDURE', 'QUALIFIED',
        'RECORD', 'REPEAT', 'RETURN', 'SET', 'THEN', 'TO', 'TYPE',
        'UNTIL', 'VAR', 'WHILE', 'WITH',
    ]

    pim_pervasives = [
        # 31 pervasives
        'ABS', 'BITSET', 'BOOLEAN', 'CAP', 'CARDINAL', 'CHAR', 'CHR', 'DEC',
        'DISPOSE', 'EXCL', 'FALSE', 'FLOAT', 'HALT', 'HIGH', 'INC', 'INCL',
        'INTEGER', 'LONGINT', 'LONGREAL', 'MAX', 'MIN', 'NEW', 'NIL', 'ODD',
        'ORD', 'PROC', 'REAL', 'SIZE', 'TRUE', 'TRUNC', 'VAL',
    ]

    iso_reserved_words = [
        # 46 reserved words
        'AND', 'ARRAY', 'BEGIN', 'BY', 'CASE', 'CONST', 'DEFINITION', 'DIV',
        'DO', 'ELSE', 'ELSIF', 'END', 'EXCEPT', 'EXIT', 'EXPORT', 'FINALLY',
        'FOR', 'FORWARD', 'FROM', 'IF', 'IMPLEMENTATION', 'IMPORT', 'IN',
        'LOOP', 'MOD', 'MODULE', 'NOT', 'OF', 'OR', 'PACKEDSET', 'POINTER',
        'PROCEDURE', 'QUALIFIED', 'RECORD', 'REPEAT', 'REM', 'RETRY',
        'RETURN', 'SET', 'THEN', 'TO', 'TYPE', 'UNTIL', 'VAR', 'WHILE',
        'WITH',
    ]

    iso_pervasives = [
        # 42 pervasives
        'ABS', 'BITSET', 'BOOLEAN', 'CAP', 'CARDINAL', 'CHAR', 'CHR', 'CMPLX',
        'COMPLEX', 'DEC', 'DISPOSE', 'EXCL', 'FALSE', 'FLOAT', 'HALT', 'HIGH',
        'IM', 'INC', 'INCL', 'INT', 'INTEGER', 'INTERRUPTIBLE', 'LENGTH',
        'LFLOAT', 'LONGCOMPLEX', 'LONGINT', 'LONGREAL', 'MAX', 'MIN', 'NEW',
        'NIL', 'ODD', 'ORD', 'PROC', 'PROTECTION', 'RE', 'REAL', 'SIZE',
        'TRUE', 'TRUNC', 'UNINTERRUBTIBLE', 'VAL',
    ]

    objm2_reserved_words = [
        # base language, 42 reserved words
        'AND', 'ARRAY', 'BEGIN', 'BY', 'CASE', 'CONST', 'DEFINITION', 'DIV',
        'DO', 'ELSE', 'ELSIF', 'END', 'ENUM', 'EXIT', 'FOR', 'FROM', 'IF',
        'IMMUTABLE', 'IMPLEMENTATION', 'IMPORT', 'IN', 'IS', 'LOOP', 'MOD',
        'MODULE', 'NOT', 'OF', 'OPAQUE', 'OR', 'POINTER', 'PROCEDURE',
        'RECORD', 'REPEAT', 'RETURN', 'SET', 'THEN', 'TO', 'TYPE',
        'UNTIL', 'VAR', 'VARIADIC', 'WHILE',
        # OO extensions, 16 reserved words
        'BYCOPY', 'BYREF', 'CLASS', 'CONTINUE', 'CRITICAL', 'INOUT', 'METHOD',
        'ON', 'OPTIONAL', 'OUT', 'PRIVATE', 'PROTECTED', 'PROTOCOL', 'PUBLIC',
        'SUPER', 'TRY',
    ]

    objm2_pervasives = [
        # base language, 38 pervasives
        'ABS', 'BITSET', 'BOOLEAN', 'CARDINAL', 'CHAR', 'CHR', 'DISPOSE',
        'FALSE', 'HALT', 'HIGH', 'INTEGER', 'INRANGE', 'LENGTH', 'LONGCARD',
        'LONGINT', 'LONGREAL', 'MAX', 'MIN', 'NEG', 'NEW', 'NEXTV', 'NIL',
        'OCTET', 'ODD', 'ORD', 'PRED', 'PROC', 'READ', 'REAL', 'SUCC', 'TMAX',
        'TMIN', 'TRUE', 'TSIZE', 'UNICHAR', 'VAL', 'WRITE', 'WRITEF',
        # OO extensions, 3 pervasives
        'OBJECT', 'NO', 'YES',
    ]

    gnu_reserved_words = [
        # 10 additional reserved words
        'ASM', '__ATTRIBUTE__', '__BUILTIN__', '__COLUMN__', '__DATE__',
        '__FILE__', '__FUNCTION__', '__LINE__', '__MODULE__', 'VOLATILE',
    ]

    gnu_pervasives = [
        # 21 identifiers, actually from pseudo-module SYSTEM
        # but we will highlight them as if they were pervasives
        'BITSET8', 'BITSET16', 'BITSET32', 'CARDINAL8', 'CARDINAL16',
        'CARDINAL32', 'CARDINAL64', 'COMPLEX32', 'COMPLEX64', 'COMPLEX96',
        'COMPLEX128', 'INTEGER8', 'INTEGER16', 'INTEGER32', 'INTEGER64',
        'REAL8', 'REAL16', 'REAL32', 'REAL96', 'REAL128', 'THROW',
    ]

    def __init__(self, **options):
        self.reserved_words = set()
        self.pervasives = set()
        # ISO Modula-2
        if get_bool_opt(options, 'iso', False):
            self.reserved_words.update(self.iso_reserved_words)
            self.pervasives.update(self.iso_pervasives)
        # Objective Modula-2
        elif get_bool_opt(options, 'objm2', False):
            self.reserved_words.update(self.objm2_reserved_words)
            self.pervasives.update(self.objm2_pervasives)
        # PIM Modula-2 (DEFAULT)
        else:
            self.reserved_words.update(self.pim_reserved_words)
            self.pervasives.update(self.pim_pervasives)
        # GNU extensions
        if get_bool_opt(options, 'gm2ext', False):
            self.reserved_words.update(self.gnu_reserved_words)
            self.pervasives.update(self.gnu_pervasives)
        # initialise
        RegexLexer.__init__(self, **options)

    def get_tokens_unprocessed(self, text):
        for index, token, value in \
            RegexLexer.get_tokens_unprocessed(self, text):
            # check for reserved words and pervasives
            if token is Name:
                if value in self.reserved_words:
                    token = Keyword.Reserved
                elif value in self.pervasives:
                    token = Keyword.Pervasive
            # return result
            yield index, token, value


class BlitzMaxLexer(RegexLexer):
    """
    For `BlitzMax <http://blitzbasic.com>`_ source code.

    *New in Pygments 1.4.*
    """

    name = 'BlitzMax'
    aliases = ['blitzmax', 'bmax']
    filenames = ['*.bmx']
    mimetypes = ['text/x-bmx']

    bmax_vopwords = r'\b(Shl|Shr|Sar|Mod)\b'
    bmax_sktypes = r'@{1,2}|[!#$%]'
    bmax_lktypes = r'\b(Int|Byte|Short|Float|Double|Long)\b'
    bmax_name = r'[a-z_][a-z0-9_]*'
    bmax_var = r'(%s)(?:(?:([ \t]*)(%s)|([ \t]*:[ \t]*\b(?:Shl|Shr|Sar|Mod)\b)|([ \t]*)([:])([ \t]*)(?:%s|(%s)))(?:([ \t]*)(Ptr))?)' % (bmax_name, bmax_sktypes, bmax_lktypes, bmax_name)
    bmax_func = bmax_var + r'?((?:[ \t]|\.\.\n)*)([(])'

    flags = re.MULTILINE | re.IGNORECASE
    tokens = {
        'root': [
            # Text
            (r'[ \t]+', Text),
            (r'\.\.\n', Text), # Line continuation
            # Comments
            (r"'.*?\n", Comment.Single),
            (r'([ \t]*)\bRem\n(\n|.)*?\s*\bEnd([ \t]*)Rem', Comment.Multiline),
            # Data types
            ('"', String.Double, 'string'),
            # Numbers
            (r'[0-9]+\.[0-9]*(?!\.)', Number.Float),
            (r'\.[0-9]*(?!\.)', Number.Float),
            (r'[0-9]+', Number.Integer),
            (r'\$[0-9a-f]+', Number.Hex),
            (r'\%[10]+', Number), # Binary
            # Other
            (r'(?:(?:(:)?([ \t]*)(:?%s|([+\-*/&|~]))|Or|And|Not|[=<>^]))' %
             (bmax_vopwords), Operator),
            (r'[(),.:\[\]]', Punctuation),
            (r'(?:#[\w \t]*)', Name.Label),
            (r'(?:\?[\w \t]*)', Comment.Preproc),
            # Identifiers
            (r'\b(New)\b([ \t]?)([(]?)(%s)' % (bmax_name),
             bygroups(Keyword.Reserved, Text, Punctuation, Name.Class)),
            (r'\b(Import|Framework|Module)([ \t]+)(%s\.%s)' %
             (bmax_name, bmax_name),
             bygroups(Keyword.Reserved, Text, Keyword.Namespace)),
            (bmax_func, bygroups(Name.Function, Text, Keyword.Type,
                                 Operator, Text, Punctuation, Text,
                                 Keyword.Type, Name.Class, Text,
                                 Keyword.Type, Text, Punctuation)),
            (bmax_var, bygroups(Name.Variable, Text, Keyword.Type, Operator,
                                Text, Punctuation, Text, Keyword.Type,
                                Name.Class, Text, Keyword.Type)),
            (r'\b(Type|Extends)([ \t]+)(%s)' % (bmax_name),
             bygroups(Keyword.Reserved, Text, Name.Class)),
            # Keywords
            (r'\b(Ptr)\b', Keyword.Type),
            (r'\b(Pi|True|False|Null|Self|Super)\b', Keyword.Constant),
            (r'\b(Local|Global|Const|Field)\b', Keyword.Declaration),
            (r'\b(TNullMethodException|TNullFunctionException|'
             r'TNullObjectException|TArrayBoundsException|'
             r'TRuntimeException)\b', Name.Exception),
            (r'\b(Strict|SuperStrict|Module|ModuleInfo|'
             r'End|Return|Continue|Exit|Public|Private|'
             r'Var|VarPtr|Chr|Len|Asc|SizeOf|Sgn|Abs|Min|Max|'
             r'New|Release|Delete|'
             r'Incbin|IncbinPtr|IncbinLen|'
             r'Framework|Include|Import|Extern|EndExtern|'
             r'Function|EndFunction|'
             r'Type|EndType|Extends|'
             r'Method|EndMethod|'
             r'Abstract|Final|'
             r'If|Then|Else|ElseIf|EndIf|'
             r'For|To|Next|Step|EachIn|'
             r'While|Wend|EndWhile|'
             r'Repeat|Until|Forever|'
             r'Select|Case|Default|EndSelect|'
             r'Try|Catch|EndTry|Throw|Assert|'
             r'Goto|DefData|ReadData|RestoreData)\b', Keyword.Reserved),
            # Final resolve (for variable names and such)
            (r'(%s)' % (bmax_name), Name.Variable),
        ],
        'string': [
            (r'""', String.Double),
            (r'"C?', String.Double, '#pop'),
            (r'[^"]+', String.Double),
        ],
    }


class NimrodLexer(RegexLexer):
    """
    For `Nimrod <http://nimrod-code.org/>`_ source code.

    *New in Pygments 1.5.*
    """

    name = 'Nimrod'
    aliases = ['nimrod', 'nim']
    filenames = ['*.nim', '*.nimrod']
    mimetypes = ['text/x-nimrod']

    flags = re.MULTILINE | re.IGNORECASE | re.UNICODE

    def underscorize(words):
        newWords = []
        new = ""
        for word in words:
            for ch in word:
                new += (ch + "_?")
            newWords.append(new)
            new = ""
        return "|".join(newWords)

    keywords = [
        'addr', 'and', 'as', 'asm', 'atomic', 'bind', 'block', 'break',
        'case', 'cast', 'const', 'continue', 'converter', 'discard',
        'distinct', 'div', 'elif', 'else', 'end', 'enum', 'except', 'finally',
        'for', 'generic', 'if', 'implies', 'in', 'yield',
        'is', 'isnot', 'iterator', 'lambda', 'let', 'macro', 'method',
        'mod', 'not', 'notin', 'object', 'of', 'or', 'out', 'proc',
        'ptr', 'raise', 'ref', 'return', 'shl', 'shr', 'template', 'try',
        'tuple', 'type' , 'when', 'while', 'with', 'without', 'xor'
    ]

    keywordsPseudo = [
        'nil', 'true', 'false'
    ]

    opWords = [
        'and', 'or', 'not', 'xor', 'shl', 'shr', 'div', 'mod', 'in',
        'notin', 'is', 'isnot'
    ]

    types = [
        'int', 'int8', 'int16', 'int32', 'int64', 'float', 'float32', 'float64',
        'bool', 'char', 'range', 'array', 'seq', 'set', 'string'
    ]

    tokens = {
        'root': [
            (r'##.*$', String.Doc),
            (r'#.*$', Comment),
            (r'\*|=|>|<|\+|-|/|@|\$|~|&|%|\!|\?|\||\\|\[|\]', Operator),
            (r'\.\.|\.|,|\[\.|\.\]|{\.|\.}|\(\.|\.\)|{|}|\(|\)|:|\^|`|;',
             Punctuation),

            # Strings
            (r'(?:[\w]+)"', String, 'rdqs'),
            (r'"""', String, 'tdqs'),
            ('"', String, 'dqs'),

            # Char
            ("'", String.Char, 'chars'),

            # Keywords
            (r'(%s)\b' % underscorize(opWords), Operator.Word),
            (r'(p_?r_?o_?c_?\s)(?![\(\[\]])', Keyword, 'funcname'),
            (r'(%s)\b' % underscorize(keywords), Keyword),
            (r'(%s)\b' % underscorize(['from', 'import', 'include']),
             Keyword.Namespace),
            (r'(v_?a_?r)\b', Keyword.Declaration),
            (r'(%s)\b' % underscorize(types), Keyword.Type),
            (r'(%s)\b' % underscorize(keywordsPseudo), Keyword.Pseudo),
            # Identifiers
            (r'\b((?![_\d])\w)(((?!_)\w)|(_(?!_)\w))*', Name),
            # Numbers
            (r'[0-9][0-9_]*(?=([eE.]|\'[fF](32|64)))',
              Number.Float, ('float-suffix', 'float-number')),
            (r'0[xX][a-fA-F0-9][a-fA-F0-9_]*', Number.Hex, 'int-suffix'),
            (r'0[bB][01][01_]*', Number, 'int-suffix'),
            (r'0o[0-7][0-7_]*', Number.Oct, 'int-suffix'),
            (r'[0-9][0-9_]*', Number.Integer, 'int-suffix'),
            # Whitespace
            (r'\s+', Text),
            (r'.+$', Error),
        ],
        'chars': [
          (r'\\([\\abcefnrtvl"\']|x[a-fA-F0-9]{2}|[0-9]{1,3})', String.Escape),
          (r"'", String.Char, '#pop'),
          (r".", String.Char)
        ],
        'strings': [
            (r'(?<!\$)\$(\d+|#|\w+)+', String.Interpol),
            (r'[^\\\'"\$\n]+', String),
            # quotes, dollars and backslashes must be parsed one at a time
            (r'[\'"\\]', String),
            # unhandled string formatting sign
            (r'\$', String)
            # newlines are an error (use "nl" state)
        ],
        'dqs': [
            (r'\\([\\abcefnrtvl"\']|\n|x[a-fA-F0-9]{2}|[0-9]{1,3})',
             String.Escape),
            (r'"', String, '#pop'),
            include('strings')
        ],
        'rdqs': [
            (r'"(?!")', String, '#pop'),
            (r'""', String.Escape),
            include('strings')
        ],
        'tdqs': [
            (r'"""(?!")', String, '#pop'),
            include('strings'),
            include('nl')
        ],
        'funcname': [
            (r'((?![\d_])\w)(((?!_)\w)|(_(?!_)\w))*', Name.Function, '#pop'),
            (r'`.+`', Name.Function, '#pop')
        ],
        'nl': [
            (r'\n', String)
        ],
        'float-number': [
          (r'\.(?!\.)[0-9_]*', Number.Float),
          (r'[eE][+-]?[0-9][0-9_]*', Number.Float),
          (r'', Text, '#pop')
        ],
        'float-suffix': [
          (r'\'[fF](32|64)', Number.Float),
          (r'', Text, '#pop')
        ],
        'int-suffix': [
          (r'\'[iI](32|64)', Number.Integer.Long),
          (r'\'[iI](8|16)', Number.Integer),
          (r'', Text, '#pop')
        ],
    }


class FantomLexer(RegexLexer):
    """
    For Fantom source code.

    *New in Pygments 1.5.*
    """
    name = 'Fantom'
    aliases = ['fan']
    filenames = ['*.fan']
    mimetypes = ['application/x-fantom']

    # often used regexes
    def s(str):
        return Template(str).substitute(
            dict (
                pod = r'[\"\w\.]+',
                eos = r'\n|;',
                id = r'[a-zA-Z_][a-zA-Z0-9_]*',
                # all chars which can be part of type definition. Starts with
                # either letter, or [ (maps), or | (funcs)
                type = r'(?:\[|[a-zA-Z_]|\|)[:\w\[\]\|\->\?]*?',
                )
            )


    tokens = {
        'comments': [
            (r'(?s)/\*.*?\*/', Comment.Multiline),           #Multiline
            (r'//.*?\n', Comment.Single),                    #Single line
            #todo: highlight references in fandocs
            (r'\*\*.*?\n', Comment.Special),                 #Fandoc
            (r'#.*\n', Comment.Single)                       #Shell-style
        ],
        'literals': [
            (r'\b-?[\d_]+(ns|ms|sec|min|hr|day)', Number),   #Duration
            (r'\b-?[\d_]*\.[\d_]+(ns|ms|sec|min|hr|day)', Number),
                                                             #Duration with dot
            (r'\b-?(\d+)?\.\d+(f|F|d|D)?', Number.Float),    #Float/Decimal
            (r'\b-?0x[0-9a-fA-F_]+', Number.Hex),            #Hex
            (r'\b-?[\d_]+', Number.Integer),                 #Int
            (r"'\\.'|'[^\\]'|'\\u[0-9a-f]{4}'", String.Char), #Char
            (r'"', Punctuation, 'insideStr'),                #Opening quote
            (r'`', Punctuation, 'insideUri'),                #Opening accent
            (r'\b(true|false|null)\b', Keyword.Constant),    #Bool & null
            (r'(?:(\w+)(::))?(\w+)(<\|)(.*?)(\|>)',          #DSL
             bygroups(Name.Namespace, Punctuation, Name.Class,
                      Punctuation, String, Punctuation)),
            (r'(?:(\w+)(::))?(\w+)?(#)(\w+)?',               #Type/slot literal
             bygroups(Name.Namespace, Punctuation, Name.Class,
                      Punctuation, Name.Function)),
            (r'\[,\]', Literal),                             # Empty list
            (s(r'($type)(\[,\])'),                           # Typed empty list
             bygroups(using(this, state = 'inType'), Literal)),
            (r'\[:\]', Literal),                             # Empty Map
            (s(r'($type)(\[:\])'),
             bygroups(using(this, state = 'inType'), Literal)),
        ],
        'insideStr': [
            (r'\\\\', String.Escape),                        #Escaped backslash
            (r'\\"', String.Escape),                         #Escaped "
            (r'\\`', String.Escape),                         #Escaped `
            (r'\$\w+', String.Interpol),                     #Subst var
            (r'\${.*?}', String.Interpol),                   #Subst expr
            (r'"', Punctuation, '#pop'),                     #Closing quot
            (r'.', String)                                   #String content
        ],
        'insideUri': [  #TODO: remove copy/paste str/uri
            (r'\\\\', String.Escape),                        #Escaped backslash
            (r'\\"', String.Escape),                         #Escaped "
            (r'\\`', String.Escape),                         #Escaped `
            (r'\$\w+', String.Interpol),                     #Subst var
            (r'\${.*?}', String.Interpol),                   #Subst expr
            (r'`', Punctuation, '#pop'),                     #Closing tick
            (r'.', String.Backtick)                          #URI content
        ],
        'protectionKeywords': [
            (r'\b(public|protected|private|internal)\b', Keyword),
        ],
        'typeKeywords': [
            (r'\b(abstract|final|const|native|facet|enum)\b', Keyword),
        ],
        'methodKeywords': [
            (r'\b(abstract|native|once|override|static|virtual|final)\b',
             Keyword),
        ],
        'fieldKeywords': [
            (r'\b(abstract|const|final|native|override|static|virtual|'
             r'readonly)\b', Keyword)
        ],
        'otherKeywords': [
            (r'\b(try|catch|throw|finally|for|if|else|while|as|is|isnot|'
             r'switch|case|default|continue|break|do|return|get|set)\b',
             Keyword),
            (r'\b(it|this|super)\b', Name.Builtin.Pseudo),
        ],
        'operators': [
            (r'\+\+|\-\-|\+|\-|\*|/|\|\||&&|<=>|<=|<|>=|>|=|!|\[|\]', Operator)
        ],
        'inType': [
            (r'[\[\]\|\->:\?]', Punctuation),
            (s(r'$id'), Name.Class),
            (r'', Text, '#pop'),

        ],
        'root': [
            include('comments'),
            include('protectionKeywords'),
            include('typeKeywords'),
            include('methodKeywords'),
            include('fieldKeywords'),
            include('literals'),
            include('otherKeywords'),
            include('operators'),
            (r'using\b', Keyword.Namespace, 'using'),         # Using stmt
            (r'@\w+', Name.Decorator, 'facet'),               # Symbol
            (r'(class|mixin)(\s+)(\w+)', bygroups(Keyword, Text, Name.Class),
             'inheritance'),                                  # Inheritance list


            ### Type var := val
            (s(r'($type)([ \t]+)($id)(\s*)(:=)'),
             bygroups(using(this, state = 'inType'), Text,
                      Name.Variable, Text, Operator)),

            ### var := val
            (s(r'($id)(\s*)(:=)'),
             bygroups(Name.Variable, Text, Operator)),

            ### .someId( or ->someId( ###
            (s(r'(\.|(?:\->))($id)(\s*)(\()'),
             bygroups(Operator, Name.Function, Text, Punctuation),
             'insideParen'),

            ### .someId  or ->someId
            (s(r'(\.|(?:\->))($id)'),
             bygroups(Operator, Name.Function)),

            ### new makeXXX ( ####
            (r'(new)(\s+)(make\w*)(\s*)(\()',
             bygroups(Keyword, Text, Name.Function, Text, Punctuation),
             'insideMethodDeclArgs'),

            ### Type name (  ####
            (s(r'($type)([ \t]+)' #Return type and whitespace
               r'($id)(\s*)(\()'), #method name + open brace
             bygroups(using(this, state = 'inType'), Text,
                      Name.Function, Text, Punctuation),
             'insideMethodDeclArgs'),

            ### ArgType argName, #####
            (s(r'($type)(\s+)($id)(\s*)(,)'),
             bygroups(using(this, state='inType'), Text, Name.Variable,
                      Text, Punctuation)),

            #### ArgType argName) ####
            ## Covered in 'insideParen' state

            ### ArgType argName -> ArgType| ###
            (s(r'($type)(\s+)($id)(\s*)(\->)(\s*)($type)(\|)'),
             bygroups(using(this, state='inType'), Text, Name.Variable,
                      Text, Punctuation, Text, using(this, state = 'inType'),
                      Punctuation)),

            ### ArgType argName|  ###
            (s(r'($type)(\s+)($id)(\s*)(\|)'),
             bygroups(using(this, state='inType'), Text, Name.Variable,
                      Text, Punctuation)),

            ### Type var
            (s(r'($type)([ \t]+)($id)'),
             bygroups(using(this, state='inType'), Text,
                      Name.Variable)),

            (r'\(', Punctuation, 'insideParen'),
            (r'\{', Punctuation, 'insideBrace'),
            (r'.', Text)
        ],
        'insideParen': [
            (r'\)', Punctuation, '#pop'),
            include('root'),
        ],
        'insideMethodDeclArgs': [
            (r'\)', Punctuation, '#pop'),
            (s(r'($type)(\s+)($id)(\s*)(\))'),
             bygroups(using(this, state='inType'), Text, Name.Variable,
                      Text, Punctuation), '#pop'),
            include('root'),
        ],
        'insideBrace': [
            (r'\}', Punctuation, '#pop'),
            include('root'),
        ],
        'inheritance': [
            (r'\s+', Text),                                      #Whitespace
            (r':|,', Punctuation),
            (r'(?:(\w+)(::))?(\w+)',
             bygroups(Name.Namespace, Punctuation, Name.Class)),
            (r'{', Punctuation, '#pop')
        ],
        'using': [
            (r'[ \t]+', Text), # consume whitespaces
            (r'(\[)(\w+)(\])',
             bygroups(Punctuation, Comment.Special, Punctuation)), #ffi
            (r'(\")?([\w\.]+)(\")?',
             bygroups(Punctuation, Name.Namespace, Punctuation)), #podname
            (r'::', Punctuation, 'usingClass'),
            (r'', Text, '#pop')
        ],
        'usingClass': [
            (r'[ \t]+', Text), # consume whitespaces
            (r'(as)(\s+)(\w+)',
             bygroups(Keyword.Declaration, Text, Name.Class), '#pop:2'),
            (r'[\w\$]+', Name.Class),
            (r'', Text, '#pop:2') # jump out to root state
        ],
        'facet': [
            (r'\s+', Text),
            (r'{', Punctuation, 'facetFields'),
            (r'', Text, '#pop')
        ],
        'facetFields': [
            include('comments'),
            include('literals'),
            include('operators'),
            (r'\s+', Text),
            (r'(\s*)(\w+)(\s*)(=)', bygroups(Text, Name, Text, Operator)),
            (r'}', Punctuation, '#pop'),
            (r'.', Text)
        ],
    }


class RustLexer(RegexLexer):
    """
    Lexer for Mozilla's Rust programming language.

    *New in Pygments 1.6.*
    """
    name = 'Rust'
    filenames = ['*.rs', '*.rc']
    aliases = ['rust']
    mimetypes = ['text/x-rustsrc']

    tokens = {
        'root': [
            # Whitespace and Comments
            (r'\n', Text),
            (r'\s+', Text),
            (r'//(.*?)\n', Comment.Single),
            (r'/[*](.|\n)*?[*]/', Comment.Multiline),

            # Keywords
            (r'(alt|as|assert|be|break|check|claim|class|const'
             r'|cont|copy|crust|do|else|enum|export|fail'
             r'|false|fn|for|if|iface|impl|import|let|log'
             r'|loop|mod|mut|native|pure|resource|ret|true'
             r'|type|unsafe|use|white|note|bind|prove|unchecked'
             r'|with|syntax|u8|u16|u32|u64|i8|i16|i32|i64|uint'
             r'|int|f32|f64)\b', Keyword),

            # Character Literal
            (r"""'(\\['"\\nrt]|\\x[0-9a-fA-F]{2}|\\[0-7]{1,3}"""
             r"""|\\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8}|.)'""",
             String.Char),
            # Binary Literal
            (r'0[Bb][01_]+', Number, 'number_lit'),
            # Octal Literal
            (r'0[0-7_]+', Number.Oct, 'number_lit'),
            # Hexadecimal Literal
            (r'0[xX][0-9a-fA-F_]+', Number.Hex, 'number_lit'),
            # Decimal Literal
            (r'[0-9][0-9_]*(\.[0-9_]+[eE][+\-]?'
             r'[0-9_]+|\.[0-9_]*|[eE][+\-]?[0-9_]+)?', Number, 'number_lit'),
            # String Literal
            (r'"', String, 'string'),

            # Operators and Punctuation
            (r'[{}()\[\],.;]', Punctuation),
            (r'[+\-*/%&|<>^!~@=:?]', Operator),

            # Identifier
            (r'[a-zA-Z_$][a-zA-Z0-9_]*', Name),

            # Attributes
            (r'#\[', Comment.Preproc, 'attribute['),
            (r'#\(', Comment.Preproc, 'attribute('),
            # Macros
            (r'#[A-Za-z_][A-Za-z0-9_]*\[', Comment.Preproc, 'attribute['),
            (r'#[A-Za-z_][A-Za-z0-9_]*\(', Comment.Preproc, 'attribute('),
        ],
        'number_lit': [
            (r'(([ui](8|16|32|64)?)|(f(32|64)?))?', Keyword, '#pop'),
        ],
        'string': [
            (r'"', String, '#pop'),
            (r"""\\['"\\nrt]|\\x[0-9a-fA-F]{2}|\\[0-7]{1,3}"""
             r"""|\\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8}""", String.Escape),
            (r'[^\\"]+', String),
            (r'\\', String),
        ],
        'attribute_common': [
            (r'"', String, 'string'),
            (r'\[', Comment.Preproc, 'attribute['),
            (r'\(', Comment.Preproc, 'attribute('),
        ],
        'attribute[': [
            include('attribute_common'),
            (r'\];?', Comment.Preproc, '#pop'),
            (r'[^"\]]+', Comment.Preproc),
        ],
        'attribute(': [
            include('attribute_common'),
            (r'\);?', Comment.Preproc, '#pop'),
            (r'[^"\)]+', Comment.Preproc),
        ],
    }


class CUDALexer(CLexer):
    """
    For NVIDIA `CUDA™ <http://developer.nvidia.com/category/zone/cuda-zone>`_
    source.

    *New in Pygments 1.6.*
    """
    name = 'CUDA'
    filenames = ['*.cu', '*.cuh']
    aliases = ['cuda', 'cu']
    mimetypes = ['text/x-cuda']

    function_qualifiers = ['__device__', '__global__', '__host__',
                           '__noinline__', '__forceinline__']
    variable_qualifiers = ['__device__', '__constant__', '__shared__',
                           '__restrict__']
    vector_types = ['char1', 'uchar1', 'char2', 'uchar2', 'char3', 'uchar3',
                    'char4', 'uchar4', 'short1', 'ushort1', 'short2', 'ushort2',
                    'short3', 'ushort3', 'short4', 'ushort4', 'int1', 'uint1',
                    'int2', 'uint2', 'int3', 'uint3', 'int4', 'uint4', 'long1',
                    'ulong1', 'long2', 'ulong2', 'long3', 'ulong3', 'long4',
                    'ulong4', 'longlong1', 'ulonglong1', 'longlong2',
                    'ulonglong2', 'float1', 'float2', 'float3', 'float4',
                    'double1', 'double2', 'dim3']
    variables = ['gridDim', 'blockIdx', 'blockDim', 'threadIdx', 'warpSize']
    functions = ['__threadfence_block', '__threadfence', '__threadfence_system',
                 '__syncthreads', '__syncthreads_count', '__syncthreads_and',
                 '__syncthreads_or']
    execution_confs = ['<<<', '>>>']

    def get_tokens_unprocessed(self, text):
        for index, token, value in \
            CLexer.get_tokens_unprocessed(self, text):
            if token is Name:
                if value in self.variable_qualifiers:
                    token = Keyword.Type
                elif value in self.vector_types:
                    token = Keyword.Type
                elif value in self.variables:
                    token = Name.Builtin
                elif value in self.execution_confs:
                    token = Keyword.Pseudo
                elif value in self.function_qualifiers:
                    token = Keyword.Reserved
                elif value in self.functions:
                    token = Name.Function
            yield index, token, value
