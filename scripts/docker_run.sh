#!/bin/bash

# Run a command in our ci container

set -e

DIR=$(cd $(dirname "${BASH_SOURCE[0]}")/.. && pwd)

if [[ $# -eq 0 ]] ; then
    COMMAND="bash"
else
    COMMAND="$@"
fi

docker run -it -v ${DIR}:/idream -P idream/ci bash -c "cd /idream && ${COMMAND}"
