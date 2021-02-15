# -*- coding: utf-8 -*-
"""
    Tests for other lexers
    ~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
import glob
import os
import unittest

from pygments.lexers import guess_lexer
from pygments.lexers.scripting import EasytrieveLexer, JclLexer, RexxLexer

def _exampleFilePath(filename):
    return os.path.join(os.path.dirname(__file__), 'examplefiles', filename)


class AnalyseTextTest(unittest.TestCase):
    def _testCanRecognizeAndGuessExampleFiles(self, lexer):
        assert lexer is not None

        for pattern in lexer.filenames:
            exampleFilesPattern = _exampleFilePath(pattern)
            for exampleFilePath in glob.glob(exampleFilesPattern):
                with open(exampleFilePath, 'rb') as fp:
                    text = fp.read().decode('utf-8')
                probability = lexer.analyse_text(text)
                self.assertTrue(probability > 0,
                    '%s must recognize %r' % (
                    lexer.name, exampleFilePath))
                guessedLexer = guess_lexer(text)
                self.assertEqual(guessedLexer.name, lexer.name)

    def testCanRecognizeAndGuessExampleFiles(self):
        LEXERS_TO_TEST = [
            EasytrieveLexer,
            JclLexer,
            RexxLexer,
        ]
        for lexerToTest in LEXERS_TO_TEST:
            self._testCanRecognizeAndGuessExampleFiles(lexerToTest)


class EasyTrieveLexerTest(unittest.TestCase):
    def testCanGuessFromText(self):
        self.assertLess(0, EasytrieveLexer.analyse_text('MACRO'))
        self.assertLess(0, EasytrieveLexer.analyse_text('\nMACRO'))
        self.assertLess(0, EasytrieveLexer.analyse_text(' \nMACRO'))
        self.assertLess(0, EasytrieveLexer.analyse_text(' \n MACRO'))
        self.assertLess(0, EasytrieveLexer.analyse_text('*\nMACRO'))
        self.assertLess(0, EasytrieveLexer.analyse_text(
            '*\n *\n\n \n*\n MACRO'))


class RexxLexerTest(unittest.TestCase):
    def testCanGuessFromText(self):
        self.assertAlmostEqual(0.01,
            RexxLexer.analyse_text('/* */'))
        self.assertAlmostEqual(1.0,
            RexxLexer.analyse_text('''/* Rexx */
                say "hello world"'''))
        val = RexxLexer.analyse_text('/* */\n'
                'hello:pRoceduRe\n'
                '  say "hello world"')
        self.assertTrue(val > 0.5, val)
        val = RexxLexer.analyse_text('''/* */
                if 1 > 0 then do
                    say "ok"
                end
                else do
                    say "huh?"
                end''')
        self.assertTrue(val > 0.2, val)
        val = RexxLexer.analyse_text('''/* */
                greeting = "hello world!"
                parse value greeting "hello" name "!"
                say name''')
        self.assertTrue(val > 0.2, val)
