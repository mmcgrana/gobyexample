# -*- coding: utf-8 -*-
"""
    Pygments unit tests
    ~~~~~~~~~~~~~~~~~~

    Usage::

        python run.py [testfile ...]


    :copyright: Copyright 2006-2013 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import sys, os

if sys.version_info >= (3,):
    # copy test suite over to "build/lib" and convert it
    print ('Copying and converting sources to build/lib/test...')
    from distutils.util import copydir_run_2to3
    testroot = os.path.dirname(__file__)
    newroot = os.path.join(testroot, '..', 'build/lib/test')
    copydir_run_2to3(testroot, newroot)
    # make nose believe that we run from the converted dir
    os.chdir(newroot)
else:
    # only find tests in this directory
    if os.path.dirname(__file__):
        os.chdir(os.path.dirname(__file__))


try:
    import nose
except ImportError:
    print ('nose is required to run the Pygments test suite')
    sys.exit(1)

try:
    # make sure the current source is first on sys.path
    sys.path.insert(0, '..')
    import pygments
except ImportError:
    print ('Cannot find Pygments to test: %s' % sys.exc_info()[1])
    sys.exit(1)
else:
    print ('Pygments %s test suite running (Python %s)...' %
           (pygments.__version__, sys.version.split()[0]))

nose.main()
