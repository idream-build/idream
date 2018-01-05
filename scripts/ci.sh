#!/bin/bash

# Basically what should go into a CI test block (after the venv is setup)

set -ex

./scripts/lint.sh
./scripts/test.sh
./scripts/demo.sh validate
./scripts/demo.sh build demo_lib
./scripts/demo.sh mkdoc demo_lib
./scripts/demo.sh build demo_bin
./scripts/demo.sh execute demo_bin
