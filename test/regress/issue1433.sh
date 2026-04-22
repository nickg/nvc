set -xe

# Forcing colours on should not emit links if the output is not a terminal
export NVC_COLORS=always

! nvc -a $TESTDIR/parse/seq.vhd 2>stderr 1>stdout

grep -F -q "parse/seq.vhd:" stderr

if ! grep -F -q "$(printf '\033[0m')" stderr stdout; then
  echo "Color escape sequence missing"
  exit 1
fi

if grep -F -q "$(printf '\033]8;;')" stderr stdout; then
  echo "Link escape sequence present"
  exit 1
fi
