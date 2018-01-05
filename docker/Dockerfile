FROM debian:stretch
RUN apt-get update && \
    apt-get install -y git zlib1g-dev python3-pip cabal-install && \
    pip3 install virtualenv
RUN cabal update && \
    cabal install --global idris-1.1.1
