#!/bin/bash

set -eux

source script/common_vars.sh

docker push ${BASE_TAG}
