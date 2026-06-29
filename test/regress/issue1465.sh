set -xe

pwd
which nvc

nvc  -a - <<'EOF'

library ieee;
use ieee.std_logic_1164.all;

package issue1465_pkg is
    type generic_positive_array is array(natural range <>) of positive;
    type generic_integer_array is array(natural range <>) of integer;
    type generic_natural_array is array(natural range <>) of natural;
end package issue1465_pkg;

use work.issue1465_pkg.all;

entity issue1465 is
    generic (
        POS_ARRAY           : generic_positive_array;
        INT_ARRAY           : generic_integer_array;
        NAT_ARRAY           : generic_natural_array
    );
end entity;

architecture beh of issue1465 is
begin
    process
    begin
        assert POS_ARRAY(0) = 1 severity failure;
        assert POS_ARRAY(1) = 2 severity failure;
        assert POS_ARRAY(2) = 3 severity failure;
        assert POS_ARRAY(3) = 4 severity failure;
        assert POS_ARRAY(4) = 8 severity failure;
        assert POS_ARRAY(5) = 6 severity failure;


        assert INT_ARRAY(0) = -1 severity failure;
        assert INT_ARRAY(1) = 0 severity failure;
        assert INT_ARRAY(2) = 3 severity failure;
        assert INT_ARRAY(3) = 4 severity failure;

        assert NAT_ARRAY(0) = 0 severity failure;
        assert NAT_ARRAY(1) = 3 severity failure;
        assert NAT_ARRAY(2) = 2 severity failure;
        wait;
    end process;
end architecture beh;
EOF

nvc $STD -e --no-save issue1465 \
    -gPOS_ARRAY='(1,2,3,4,8,6)' \
    -gINT_ARRAY='(-1,0,3,4)' \
    -gNAT_ARRAY='(0,3,2)' \
    -r

! nvc $STD -e --no-save issue1465 \
    -gPOS_ARRAY='(1,2,)' \
    -gINT_ARRAY='(-1,0,3,4)' \
    -gNAT_ARRAY='(0,3,2)' \
    2>pos_array_trailing.stderr
grep -q 'failed to parse "(1,2,)" as type GENERIC_POSITIVE_ARRAY for generic POS_ARRAY' pos_array_trailing.stderr

! nvc $STD -e --no-save issue1465 \
    -gPOS_ARRAY='()' \
    -gINT_ARRAY='(-1,0,3,4)' \
    -gNAT_ARRAY='(0,3,2)' \
    2>pos_array_empty.stderr
grep -q 'failed to parse "()" as type GENERIC_POSITIVE_ARRAY for generic POS_ARRAY' pos_array_empty.stderr
