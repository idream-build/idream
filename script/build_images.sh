#!/bin/bash

set -eux

source script/common_vars.sh

# Prepare idream binary
docker run -v "${HOME}/.stack/snapshots":/root/.stack/snapshots -v "${PWD}":/project -w /project -it ejconlon/haskell-custom:8.10.4 bash images/prepare.sh

# Build base image
pushd images/base
  docker build -t ${BASE_TAG} --build-arg VERSION=${IDRIS_VERSION} .
popd

# Build integration image
pushd images/integration
  docker build -t ${INTEGRATION_TAG} --build-arg BASE_TAG=${BASE_TAG} .
popd
