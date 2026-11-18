set -xe

nvc -a $TESTDIR/regress/genpath2.vhd \
    -e --generic-default='*.W=5' --generic-default=c.u.W=4 genpath2 -r

# A -g override wins over a default and replaces the generic map value
nvc -e --generic-default='*.W=5' -gb.W=7 genpath2 -r 2>&1 | tee out.txt
grep -q "genpath2:b: W=7 expected 3" out.txt
