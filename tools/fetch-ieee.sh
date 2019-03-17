#!/bin/sh
set -e

ieee_url="https://standards.ieee.org/content/dam/ieee-standards/standards/web/download/"

cat <<EOF
The IEEE standard library sources are now included in the repository for 
convenience. They can also be download from the IEEE SA:

  $ieee_url

Note that the license forbids the distribution of modifications.
EOF
