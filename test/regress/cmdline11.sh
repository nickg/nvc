set -xe

pwd
which nvc

! nvc -e - 2>err

grep "'-' is not a valid design unit name" err
