#!/bin/bash
set -e -o pipefail

cd $(dirname ${BASH_SOURCE[0]})/../..

vers=$(sed -n '1s/AC_INIT(\[.*\], \[\([0-9\.]*\).*\].*/\1/p' configure.ac)

docker build -f contrib/docker/Dockerfile . \
       --build-arg VERSION=$vers \
       --tag ghcr.io/nickg/nvc:latest
