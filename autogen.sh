#!/bin/sh

# Required for OpenBSD
export AUTOCONF_VERSION=2.69
export AUTOMAKE_VERSION=1.15

cd $(git rev-parse --show-toplevel)
autoreconf --force --install -I m4
