set -xe

pwd
which nvc

nvc -a --no-save $TESTDIR/regress/wait1.vhd

! [ -f work/WORK.WAIT1 ]
