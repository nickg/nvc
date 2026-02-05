set -x

nvc whatever 2>err

code=$?

if [ "$code" != 1 ]; then
  echo "unexpected exit code $code"
  exit 1
fi

set -e

grep "missing command" err
grep -v "Caught signal" err

