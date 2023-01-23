set -xe

pwd
which nvc

! nvc --bad-option 2>err
! nvc -r -x 2>>err
! nvc -r -w --format 2>>err

cat err

diff -u - err <<EOF
** Fatal: unrecognised global option --bad-option
** Fatal: unrecognised run option -x
** Fatal: run option --format requires an argument
EOF
