set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/signal1.vhd

! nvc -e --cover --cover-file=dir1/test.ncdb signal1 2>err

grep "failed to open coverage database: dir1/test.ncdb" err
