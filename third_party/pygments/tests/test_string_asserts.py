# -*- coding: utf-8 -*-
"""
    Pygments string assert utility tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest
from string_asserts import StringTests

class TestStringTests(StringTests, unittest.TestCase):

    def test_startswith_correct(self):
        self.assertStartsWith("AAA", "A")

    # @unittest.expectedFailure not supported by nose
    def test_startswith_incorrect(self):
        self.assertRaises(AssertionError, self.assertStartsWith, "AAA", "B")

    # @unittest.expectedFailure not supported by nose
    def test_startswith_short(self):
        self.assertRaises(AssertionError, self.assertStartsWith, "A", "AA")

    def test_endswith_correct(self):
        self.assertEndsWith("AAA", "A")

    # @unittest.expectedFailure not supported by nose
    def test_endswith_incorrect(self):
        self.assertRaises(AssertionError, self.assertEndsWith, "AAA", "B")

    # @unittest.expectedFailure not supported by nose
    def test_endswith_short(self):
        self.assertRaises(AssertionError, self.assertEndsWith, "A", "AA")
