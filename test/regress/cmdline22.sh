set -xe

# Analyse again after elaborating in the same command
nvc -a $TESTDIR/regress/tcl1.vhd -e tcl1 -a $TESTDIR/regress/tcl1.vhd -e tcl1 -r
