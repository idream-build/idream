#!/bin/bash

# Run idream out of the venv

cd demo

exec ../.venv/bin/python -m idream $@
