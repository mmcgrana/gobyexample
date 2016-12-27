# -*- coding: utf-8 -*-
"""
    Test suite for the token module
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import copy
import unittest

from pygments import token


class TokenTest(unittest.TestCase):

    def test_tokentype(self):
        e = self.assertEqual

        t = token.String

        e(t.split(), [token.Token, token.Literal, token.String])

        e(t.__class__, token._TokenType)

    def test_functions(self):
        self.assertTrue(token.is_token_subtype(token.String, token.String))
        self.assertTrue(token.is_token_subtype(token.String, token.Literal))
        self.assertFalse(token.is_token_subtype(token.Literal, token.String))

        self.assertTrue(token.string_to_tokentype(token.String) is token.String)
        self.assertTrue(token.string_to_tokentype('') is token.Token)
        self.assertTrue(token.string_to_tokentype('String') is token.String)

    def test_sanity_check(self):
        stp = token.STANDARD_TYPES.copy()
        stp[token.Token] = '---' # Token and Text do conflict, that is okay
        t = {}
        for k, v in stp.items():
            t.setdefault(v, []).append(k)
        if len(t) == len(stp):
            return # Okay

        for k, v in t.items():
            if len(v) > 1:
                self.fail("%r has more than one key: %r" % (k, v))

    def test_copying(self):
        # Token instances are supposed to be singletons, so copying or even
        # deepcopying should return themselves
        t = token.String
        self.assertIs(t, copy.copy(t))
        self.assertIs(t, copy.deepcopy(t))
