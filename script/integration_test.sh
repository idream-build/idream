#!/bin/bash

set -eux

source script/common_vars.sh

docker run -v "${PWD}":/project -w /project -it ${INTEGRATION_TAG} bash integration/entrypoint.sh
