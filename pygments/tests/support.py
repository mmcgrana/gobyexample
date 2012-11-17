# coding: utf-8
"""
Support for Pygments tests
"""

import os


def location(mod_name):
    """
    Return the file and directory that the code for *mod_name* is in.
    """
    source = mod_name.endswith("pyc") and mod_name[:-1] or mod_name
    source = os.path.abspath(source)
    return source, os.path.dirname(source)
