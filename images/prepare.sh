#!/bin/bash

set -eux

DEST="images/base/cache"
rm -rf ${DEST}
mkdir ${DEST}

stack install idream

INSTALL_DIR="$(stack path --local-install-root)"

cp "${INSTALL_DIR}/bin/idream" ${DEST}/idream
