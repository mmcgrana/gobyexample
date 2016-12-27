# -*- coding: utf-8 -*-
"""
    Tests for pygments.regexopt
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re
import random
import unittest
import itertools

from pygments.regexopt import regex_opt

ALPHABET = ['a', 'b', 'c', 'd', 'e']

try:
    from itertools import combinations_with_replacement
    N_TRIES = 15
except ImportError:
    # Python 2.6
    def combinations_with_replacement(iterable, r):
        pool = tuple(iterable)
        n = len(pool)
        for indices in itertools.product(range(n), repeat=r):
            if sorted(indices) == list(indices):
                yield tuple(pool[i] for i in indices)
    N_TRIES = 9


class RegexOptTestCase(unittest.TestCase):

    def generate_keywordlist(self, length):
        return [''.join(p) for p in
                combinations_with_replacement(ALPHABET, length)]

    def test_randomly(self):
        # generate a list of all possible keywords of a certain length using
        # a restricted alphabet, then choose some to match and make sure only
        # those do
        for n in range(3, N_TRIES):
            kwlist = self.generate_keywordlist(n)
            to_match = random.sample(kwlist,
                                     random.randint(1, len(kwlist) - 1))
            no_match = set(kwlist) - set(to_match)
            rex = re.compile(regex_opt(to_match))
            for w in to_match:
                self.assertTrue(rex.match(w))
            for w in no_match:
                self.assertFalse(rex.match(w))

    def test_prefix(self):
        opt = regex_opt(('a', 'b'), prefix=r':{1,2}')
        print(opt)
        rex = re.compile(opt)
        self.assertFalse(rex.match('a'))
        self.assertTrue(rex.match('::a'))
        self.assertFalse(rex.match(':::')) # fullmatch

    def test_suffix(self):
        opt = regex_opt(('a', 'b'), suffix=r':{1,2}')
        print(opt)
        rex = re.compile(opt)
        self.assertFalse(rex.match('a'))
        self.assertTrue(rex.match('a::'))
        self.assertFalse(rex.match(':::')) # fullmatch

    def test_suffix_opt(self):
        # test that detected suffixes remain sorted.
        opt = regex_opt(('afoo', 'abfoo'))
        print(opt)
        rex = re.compile(opt)
        m = rex.match('abfoo')
        self.assertEqual(5, m.end())
