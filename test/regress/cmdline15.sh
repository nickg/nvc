set -xe

nvc -a $TESTDIR/regress/tcl1.vhd -e tcl1

nvc --do tcl1 $TESTDIR/regress/tcl1.tcl

nvc -i tcl1 < $TESTDIR/regress/tcl1.tcl
