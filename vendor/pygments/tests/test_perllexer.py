# -*- coding: utf-8 -*-
"""
    Pygments regex lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2013 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import time
import unittest

from pygments.token import String
from pygments.lexers.agile import PerlLexer


class RunawayRegexTest(unittest.TestCase):
    # A previous version of the Perl lexer would spend a great deal of
    # time backtracking when given particular strings.  These tests show that
    # the runaway backtracking doesn't happen any more (at least for the given
    # cases).

    lexer = PerlLexer()

    ### Test helpers.

    def assert_single_token(self, s, token):
        """Show that a given string generates only one token."""
        tokens = list(self.lexer.get_tokens_unprocessed(s))
        self.assertEqual(len(tokens), 1, tokens)
        self.assertEqual(s, tokens[0][2])
        self.assertEqual(token, tokens[0][1])

    def assert_tokens(self, strings, expected_tokens):
        """Show that a given string generates the expected tokens."""
        tokens = list(self.lexer.get_tokens_unprocessed(''.join(strings)))
        self.assertEqual(len(tokens), len(expected_tokens), tokens)
        for index, s in enumerate(strings):
            self.assertEqual(s, tokens[index][2])
            self.assertEqual(expected_tokens[index], tokens[index][1])

    def assert_fast_tokenization(self, s):
        """Show that a given string is tokenized quickly."""
        start = time.time()
        tokens = list(self.lexer.get_tokens_unprocessed(s))
        end = time.time()
        # Isn't 10 seconds kind of a long time?  Yes, but we don't want false
        # positives when the tests are starved for CPU time.
        if end-start > 10:
            self.fail('tokenization took too long')
        return tokens

    ### Strings.

    def test_single_quote_strings(self):
        self.assert_single_token(r"'foo\tbar\\\'baz'", String)
        self.assert_fast_tokenization("'" + '\\'*999)

    def test_double_quote_strings(self):
        self.assert_single_token(r'"foo\tbar\\\"baz"', String)
        self.assert_fast_tokenization('"' + '\\'*999)

    def test_backtick_strings(self):
        self.assert_single_token(r'`foo\tbar\\\`baz`', String.Backtick)
        self.assert_fast_tokenization('`' + '\\'*999)

    ### Regex matches with various delimiters.

    def test_match(self):
        self.assert_single_token(r'/aa\tbb/', String.Regex)
        self.assert_fast_tokenization('/' + '\\'*999)

    def test_match_with_slash(self):
        self.assert_tokens(['m', '/\n\\t\\\\/'], [String.Regex, String.Regex])
        self.assert_fast_tokenization('m/xxx\n' + '\\'*999)

    def test_match_with_bang(self):
        self.assert_tokens(['m', r'!aa\t\!bb!'], [String.Regex, String.Regex])
        self.assert_fast_tokenization('m!' + '\\'*999)

    def test_match_with_brace(self):
        self.assert_tokens(['m', r'{aa\t\}bb}'], [String.Regex, String.Regex])
        self.assert_fast_tokenization('m{' + '\\'*999)

    def test_match_with_angle_brackets(self):
        self.assert_tokens(['m', r'<aa\t\>bb>'], [String.Regex, String.Regex])
        self.assert_fast_tokenization('m<' + '\\'*999)

    def test_match_with_parenthesis(self):
        self.assert_tokens(['m', r'(aa\t\)bb)'], [String.Regex, String.Regex])
        self.assert_fast_tokenization('m(' + '\\'*999)

    def test_match_with_at_sign(self):
        self.assert_tokens(['m', r'@aa\t\@bb@'], [String.Regex, String.Regex])
        self.assert_fast_tokenization('m@' + '\\'*999)

    def test_match_with_percent_sign(self):
        self.assert_tokens(['m', r'%aa\t\%bb%'], [String.Regex, String.Regex])
        self.assert_fast_tokenization('m%' + '\\'*999)

    def test_match_with_dollar_sign(self):
        self.assert_tokens(['m', r'$aa\t\$bb$'], [String.Regex, String.Regex])
        self.assert_fast_tokenization('m$' + '\\'*999)

    ### Regex substitutions with various delimeters.

    def test_substitution_with_slash(self):
        self.assert_single_token('s/aaa/bbb/g', String.Regex)
        self.assert_fast_tokenization('s/foo/' + '\\'*999)

    def test_substitution_with_at_sign(self):
        self.assert_single_token(r's@aaa@bbb@g', String.Regex)
        self.assert_fast_tokenization('s@foo@' + '\\'*999)

    def test_substitution_with_percent_sign(self):
        self.assert_single_token(r's%aaa%bbb%g', String.Regex)
        self.assert_fast_tokenization('s%foo%' + '\\'*999)

    def test_substitution_with_brace(self):
        self.assert_single_token(r's{aaa}', String.Regex)
        self.assert_fast_tokenization('s{' + '\\'*999)

    def test_substitution_with_angle_bracket(self):
        self.assert_single_token(r's<aaa>', String.Regex)
        self.assert_fast_tokenization('s<' + '\\'*999)

    def test_substitution_with_angle_bracket(self):
        self.assert_single_token(r's<aaa>', String.Regex)
        self.assert_fast_tokenization('s<' + '\\'*999)

    def test_substitution_with_square_bracket(self):
        self.assert_single_token(r's[aaa]', String.Regex)
        self.assert_fast_tokenization('s[' + '\\'*999)

    def test_substitution_with_parenthesis(self):
        self.assert_single_token(r's(aaa)', String.Regex)
        self.assert_fast_tokenization('s(' + '\\'*999)
