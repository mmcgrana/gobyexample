#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Pygments
    ~~~~~~~~

    Pygments is a syntax highlighting package written in Python.

    It is a generic syntax highlighter suitable for use in code hosting, forums,
    wikis or other applications that need to prettify source code.  Highlights
    are:

    * a wide range of over 300 languages and other text formats is supported
    * special attention is paid to details, increasing quality by a fair amount
    * support for new languages and formats are added easily
    * a number of output formats, presently HTML, LaTeX, RTF, SVG, all image \
      formats that PIL supports and ANSI sequences
    * it is usable as a command-line tool and as a library

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

try:
    from setuptools import setup, find_packages
    have_setuptools = True
except ImportError:
    try:
        import ez_setup
        ez_setup.use_setuptools()
        from setuptools import setup, find_packages
        have_setuptools = True
    except ImportError:
        from distutils.core import setup
        def find_packages(*args, **kwargs):
            return [
                'pygments',
                'pygments.lexers',
                'pygments.formatters',
                'pygments.styles',
                'pygments.filters',
            ]
        have_setuptools = False

if have_setuptools:
    add_keywords = dict(
        entry_points = {
            'console_scripts': ['pygmentize = pygments.cmdline:main'],
        },
    )
else:
    add_keywords = dict(
        scripts = ['pygmentize'],
    )

setup(
    name = 'Pygments',
    version = '2.1.3',
    url = 'http://pygments.org/',
    license = 'BSD License',
    author = 'Georg Brandl',
    author_email = 'georg@python.org',
    description = 'Pygments is a syntax highlighting package written in Python.',
    long_description = __doc__,
    keywords = 'syntax highlighting',
    packages = find_packages(exclude=['ez_setup']),
    platforms = 'any',
    zip_safe = False,
    include_package_data = True,
    classifiers = [
        'License :: OSI Approved :: BSD License',
        'Intended Audience :: Developers',
        'Intended Audience :: End Users/Desktop',
        'Intended Audience :: System Administrators',
        'Development Status :: 6 - Mature',
        'Programming Language :: Python',
        'Programming Language :: Python :: 2',
        'Programming Language :: Python :: 3',
        'Operating System :: OS Independent',
        'Topic :: Text Processing :: Filters',
        'Topic :: Utilities',
    ],
    **add_keywords
)
