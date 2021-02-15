# -*- coding: utf-8 -*-
"""
    Pygments string assert utility
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

class StringTests(object):

    def assertStartsWith(self, haystack, needle, msg=None):
        if msg is None:
            msg = "'{0}' does not start with '{1}'".format(haystack, needle)
        if not haystack.startswith(needle):
            raise(AssertionError(msg))

    def assertEndsWith(self, haystack, needle, msg=None):
        if msg is None:
            msg = "'{0}' does not end with '{1}'".format(haystack, needle)
        if not haystack.endswith(needle):
            raise(AssertionError(msg))
