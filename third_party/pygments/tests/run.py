# -*- coding: utf-8 -*-
"""
    Pygments unit tests
    ~~~~~~~~~~~~~~~~~~

    Usage::

        python run.py [testfile ...]


    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from __future__ import print_function

import os
import sys

# only find tests in this directory
if os.path.dirname(__file__):
    os.chdir(os.path.dirname(__file__))


try:
    import nose
except ImportError:
    print('nose is required to run the Pygments test suite')
    sys.exit(1)

# make sure the current source is first on sys.path
sys.path.insert(0, '..')

if '--with-coverage' not in sys.argv:
    # if running with coverage, pygments should not be imported before coverage
    # is started, otherwise it will count already executed lines as uncovered
    try:
        import pygments
    except ImportError as err:
        print('Cannot find Pygments to test: %s' % err)
        sys.exit(1)
    else:
        print('Pygments %s test suite running (Python %s)...' %
              (pygments.__version__, sys.version.split()[0]),
              file=sys.stderr)
else:
    print('Pygments test suite running (Python %s)...' % sys.version.split()[0],
          file=sys.stderr)

nose.main()
