#!/bin/bash
set -eu -o pipefail

cd $(dirname ${BASH_SOURCE[0]})

for vers in 22.04 24.04; do
  docker build . \
         --build-arg VERSION=$vers \
         --tag ghcr.io/nickg/nvc-ubuntu-builder:$vers

  docker push ghcr.io/nickg/nvc-ubuntu-builder:$vers
done
