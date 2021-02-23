#!/bin/sh
cd $(git rev-parse --show-toplevel)
autoreconf --force --install -I m4
