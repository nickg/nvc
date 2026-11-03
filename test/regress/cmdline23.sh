set -xe

# Analyse again after a script in the same command
nvc -a $TESTDIR/regress/tcl1.vhd -e tcl1 --do tcl1 $TESTDIR/regress/tcl1.tcl \
    -a $TESTDIR/regress/tcl1.vhd -e tcl1 -r
