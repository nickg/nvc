set -xe

# --notimingchecks at analysis: should drop T-check
nvc --notimingchecks -a $TESTDIR/regress/specify6.v
nvc -e specify6 -r 2>&1 | tee out1
if grep "timing check violation" out1; then
    exit 1
fi

# --notimingchecks at elaboration: should not emit T-Check
nvc -a $TESTDIR/regress/specify6.v
nvc --notimingchecks -e specify6 -r 2>&1 | tee out2
if grep "timing check violation" out2; then
    exit 1
fi

# --notimingchecks at run-time: check emitted but suppressed when it fires
nvc -a $TESTDIR/regress/specify6.v -e specify6
nvc --notimingchecks -r specify6 2>&1 | tee out3
if grep "timing check violation" out3; then
    exit 1
fi
