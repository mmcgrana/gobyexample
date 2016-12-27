# -*- coding: utf-8 -*-
"""
    Tests for inheritance in RegexLexer
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.lexer import RegexLexer, inherit
from pygments.token import Text


class InheritTest(unittest.TestCase):
    def test_single_inheritance_position(self):
        t = Two()
        pats = [x[0].__self__.pattern for x in t._tokens['root']]
        self.assertEqual(['x', 'a', 'b', 'y'], pats)
    def test_multi_inheritance_beginning(self):
        t = Beginning()
        pats = [x[0].__self__.pattern for x in t._tokens['root']]
        self.assertEqual(['x', 'a', 'b', 'y', 'm'], pats)
    def test_multi_inheritance_end(self):
        t = End()
        pats = [x[0].__self__.pattern for x in t._tokens['root']]
        self.assertEqual(['m', 'x', 'a', 'b', 'y'], pats)

    def test_multi_inheritance_position(self):
        t = Three()
        pats = [x[0].__self__.pattern for x in t._tokens['root']]
        self.assertEqual(['i', 'x', 'a', 'b', 'y', 'j'], pats)

    def test_single_inheritance_with_skip(self):
        t = Skipped()
        pats = [x[0].__self__.pattern for x in t._tokens['root']]
        self.assertEqual(['x', 'a', 'b', 'y'], pats)


class One(RegexLexer):
    tokens = {
        'root': [
            ('a', Text),
            ('b', Text),
        ],
    }

class Two(One):
    tokens = {
        'root': [
            ('x', Text),
            inherit,
            ('y', Text),
        ],
    }

class Three(Two):
    tokens = {
        'root': [
            ('i', Text),
            inherit,
            ('j', Text),
        ],
    }

class Beginning(Two):
    tokens = {
        'root': [
            inherit,
            ('m', Text),
        ],
    }

class End(Two):
    tokens = {
        'root': [
            ('m', Text),
            inherit,
        ],
    }

class Empty(One):
    tokens = {}

class Skipped(Empty):
    tokens = {
        'root': [
            ('x', Text),
            inherit,
            ('y', Text),
        ],
    }

