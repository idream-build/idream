#!/bin/bash

set -e

virtualenv .venv --python=python3

.venv/bin/pip install -r idream/requirements.develop.txt
.venv/bin/pip install -e idream
