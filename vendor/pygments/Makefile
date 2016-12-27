#
# Makefile for Pygments
# ~~~~~~~~~~~~~~~~~~~~~
#
# Combines scripts for common tasks.
#
# :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
# :license: BSD, see LICENSE for details.
#

PYTHON ?= python

export PYTHONPATH = $(shell echo "$$PYTHONPATH"):$(shell python -c 'import os; print ":".join(os.path.abspath(line.strip()) for line in file("PYTHONPATH"))' 2>/dev/null)

.PHONY: all check clean clean-pyc codetags docs mapfiles \
	pylint reindent test test-coverage

all: clean-pyc check test

check:
	@$(PYTHON) scripts/detect_missing_analyse_text.py || true
	@pyflakes pygments | grep -v 'but unused' || true
	@$(PYTHON) scripts/check_sources.py -i build -i dist -i pygments/lexers/_mapping.py \
		   -i docs/build -i pygments/formatters/_mapping.py -i pygments/unistring.py

clean: clean-pyc
	-rm -rf build
	-rm -f codetags.html

clean-pyc:
	find . -name '*.pyc' -exec rm -f {} +
	find . -name '*.pyo' -exec rm -f {} +
	find . -name '*~' -exec rm -f {} +

codetags:
	@$(PYTHON) scripts/find_codetags.py -i tests/examplefiles -i scripts/pylintrc \
		   -i scripts/find_codetags.py -o codetags.html .

docs:
	make -C doc html

mapfiles:
	(cd pygments/formatters; $(PYTHON) _mapping.py)
	(cd pygments/lexers; $(PYTHON) _mapping.py)

pylint:
	@pylint --rcfile scripts/pylintrc pygments

reindent:
	@$(PYTHON) scripts/reindent.py -r -B .

test:
	@$(PYTHON) tests/run.py -d $(TEST)

test-coverage:
	@$(PYTHON) tests/run.py -d --with-coverage --cover-package=pygments --cover-erase $(TEST)

tox-test:
	@tox -- $(TEST)

tox-test-coverage:
	@tox -- --with-coverage --cover-package=pygments --cover-erase $(TEST)
