#!/bin/bash

set -eux

PROJECT="$1"
shift

pushd ${PROJECT}
  make $@
popd
