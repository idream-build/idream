#!/bin/bash

set -e

virtualenv .venv

.venv/bin/pip install -e idream
