#!/bin/bash

# Upload idream to pypi

set -ex

cd idream

../.venv/bin/python setup.py bdist_wheel

../.venv/bin/twine upload dist/*
