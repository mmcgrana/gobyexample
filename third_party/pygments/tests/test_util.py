# -*- coding: utf-8 -*-
"""
    Test suite for the util module
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re
import unittest

from pygments import util, console


class FakeLexer(object):
    def analyse(text):
        return text
    analyse = util.make_analysator(analyse)


class UtilTest(unittest.TestCase):

    def test_getoptions(self):
        raises = self.assertRaises
        equals = self.assertEqual

        equals(util.get_bool_opt({}, 'a', True), True)
        equals(util.get_bool_opt({}, 'a', 1), True)
        equals(util.get_bool_opt({}, 'a', 'true'), True)
        equals(util.get_bool_opt({}, 'a', 'no'), False)
        raises(util.OptionError, util.get_bool_opt, {}, 'a', [])
        raises(util.OptionError, util.get_bool_opt, {}, 'a', 'foo')

        equals(util.get_int_opt({}, 'a', 1), 1)
        raises(util.OptionError, util.get_int_opt, {}, 'a', [])
        raises(util.OptionError, util.get_int_opt, {}, 'a', 'bar')

        equals(util.get_list_opt({}, 'a', [1]), [1])
        equals(util.get_list_opt({}, 'a', '1 2'), ['1', '2'])
        raises(util.OptionError, util.get_list_opt, {}, 'a', 1)

        equals(util.get_choice_opt({}, 'a', ['foo', 'bar'], 'bar'), 'bar')
        equals(util.get_choice_opt({}, 'a', ['foo', 'bar'], 'Bar', True), 'bar')
        raises(util.OptionError, util.get_choice_opt, {}, 'a',
               ['foo', 'bar'], 'baz')

    def test_docstring_headline(self):
        def f1():
            """
            docstring headline

            other text
            """
        def f2():
            """
            docstring
            headline

            other text
            """
        def f3():
            pass

        self.assertEqual(util.docstring_headline(f1), 'docstring headline')
        self.assertEqual(util.docstring_headline(f2), 'docstring headline')
        self.assertEqual(util.docstring_headline(f3), '')

    def test_analysator_returns_float(self):
        # If an analysator wrapped by make_analysator returns a floating point
        # number, then that number will be returned by the wrapper.
        self.assertEqual(FakeLexer.analyse('0.5'), 0.5)

    def test_analysator_returns_boolean(self):
        # If an analysator wrapped by make_analysator returns a boolean value,
        # then the wrapper will return 1.0 if the boolean was True or 0.0 if
        # it was False.
        self.assertEqual(FakeLexer.analyse(True), 1.0)
        self.assertEqual(FakeLexer.analyse(False), 0.0)

    def test_analysator_raises_exception(self):
        # If an analysator wrapped by make_analysator raises an exception,
        # then the wrapper will return 0.0.
        class ErrorLexer(object):
            def analyse(text):
                raise RuntimeError('something bad happened')
            analyse = util.make_analysator(analyse)
        self.assertEqual(ErrorLexer.analyse(''), 0.0)

    def test_analysator_value_error(self):
        # When converting the analysator's return value to a float a
        # ValueError may occur.  If that happens 0.0 is returned instead.
        self.assertEqual(FakeLexer.analyse('bad input'), 0.0)

    def test_analysator_type_error(self):
        # When converting the analysator's return value to a float a
        # TypeError may occur.  If that happens 0.0 is returned instead.
        self.assertEqual(FakeLexer.analyse('xxx'), 0.0)

    def test_shebang_matches(self):
        self.assertTrue(util.shebang_matches('#!/usr/bin/env python\n', r'python(2\.\d)?'))
        self.assertTrue(util.shebang_matches('#!/usr/bin/python2.4', r'python(2\.\d)?'))
        self.assertTrue(util.shebang_matches('#!/usr/bin/startsomethingwith python',
                                             r'python(2\.\d)?'))
        self.assertTrue(util.shebang_matches('#!C:\\Python2.4\\Python.exe',
                                             r'python(2\.\d)?'))

        self.assertFalse(util.shebang_matches('#!/usr/bin/python-ruby',
                                              r'python(2\.\d)?'))
        self.assertFalse(util.shebang_matches('#!/usr/bin/python/ruby',
                                              r'python(2\.\d)?'))
        self.assertFalse(util.shebang_matches('#!', r'python'))

    def test_doctype_matches(self):
        self.assertTrue(util.doctype_matches(
            '<!DOCTYPE html> <html>', 'html.*'))
        self.assertFalse(util.doctype_matches(
            '<?xml ?> <DOCTYPE html PUBLIC "a"> <html>', 'html.*'))
        self.assertTrue(util.html_doctype_matches(
            '<?xml ?><!DOCTYPE html PUBLIC  "-//W3C//DTD XHTML 1.0 Strict//EN">'))

    def test_xml(self):
        self.assertTrue(util.looks_like_xml(
            '<?xml ?><!DOCTYPE html PUBLIC  "-//W3C//DTD XHTML 1.0 Strict//EN">'))
        self.assertTrue(util.looks_like_xml('<html xmlns>abc</html>'))
        self.assertFalse(util.looks_like_xml('<html>'))

    def test_unirange(self):
        first_non_bmp = u'\U00010000'
        r = re.compile(util.unirange(0x10000, 0x20000))
        m = r.match(first_non_bmp)
        self.assertTrue(m)
        self.assertEqual(m.end(), len(first_non_bmp))
        self.assertFalse(r.match(u'\uffff'))
        self.assertFalse(r.match(u'xxx'))
        # Tests that end is inclusive
        r = re.compile(util.unirange(0x10000, 0x10000) + '+')
        # Tests that the plus works for the entire unicode point, if narrow
        # build
        m = r.match(first_non_bmp * 2)
        self.assertTrue(m)
        self.assertEqual(m.end(), len(first_non_bmp) * 2)

    def test_format_lines(self):
        lst = ['cat', 'dog']
        output = util.format_lines('var', lst)
        d = {}
        exec(output, d)
        self.assertTrue(isinstance(d['var'], tuple))
        self.assertEqual(('cat', 'dog'), d['var'])

    def test_duplicates_removed_seq_types(self):
        # tuple
        x = util.duplicates_removed(('a', 'a', 'b'))
        self.assertEqual(['a', 'b'], x)
        # list
        x = util.duplicates_removed(['a', 'a', 'b'])
        self.assertEqual(['a', 'b'], x)
        # iterator
        x = util.duplicates_removed(iter(('a', 'a', 'b')))
        self.assertEqual(['a', 'b'], x)

    def test_duplicates_removed_nonconsecutive(self):
        # keeps first
        x = util.duplicates_removed(('a', 'b', 'a'))
        self.assertEqual(['a', 'b'], x)

    def test_guess_decode(self):
        # UTF-8 should be decoded as UTF-8
        s = util.guess_decode(u'\xff'.encode('utf-8'))
        self.assertEqual(s, (u'\xff', 'utf-8'))

        # otherwise, it could be latin1 or the locale encoding...
        import locale
        s = util.guess_decode(b'\xff')
        self.assertTrue(s[1] in ('latin1', locale.getpreferredencoding()))

    def test_guess_decode_from_terminal(self):
        class Term:
            encoding = 'utf-7'

        s = util.guess_decode_from_terminal(u'\xff'.encode('utf-7'), Term)
        self.assertEqual(s, (u'\xff', 'utf-7'))

        s = util.guess_decode_from_terminal(u'\xff'.encode('utf-8'), Term)
        self.assertEqual(s, (u'\xff', 'utf-8'))

    def test_add_metaclass(self):
        class Meta(type):
            pass

        @util.add_metaclass(Meta)
        class Cls:
            pass

        self.assertEqual(type(Cls), Meta)


class ConsoleTest(unittest.TestCase):

    def test_ansiformat(self):
        f = console.ansiformat
        c = console.codes
        all_attrs = f('+*_blue_*+', 'text')
        self.assertTrue(c['blue'] in all_attrs and c['blink'] in all_attrs
                        and c['bold'] in all_attrs and c['underline'] in all_attrs
                        and c['reset'] in all_attrs)
        self.assertRaises(KeyError, f, '*mauve*', 'text')

    def test_functions(self):
        self.assertEqual(console.reset_color(), console.codes['reset'])
        self.assertEqual(console.colorize('blue', 'text'),
                         console.codes['blue'] + 'text' + console.codes['reset'])
