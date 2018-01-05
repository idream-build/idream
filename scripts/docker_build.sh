#!/bin/bash

# Build a docker container for ci (requires >= 3GB memory assigned to docker)

set -e

cd docker

docker build -t idream/ci .
