set -xe

pwd
which nvc

nvc -a - <<'EOF'
package issue1465_pkg is
    type int_vector_t  is array (natural range <>) of integer;
    type nat_vector_t  is array (natural range <>) of natural;
    type real_vector_t is array (natural range <>) of real;
    type time_vector_t is array (natural range <>) of time;
    type bv_vector_t   is array (natural range <>) of bit_vector(3 downto 0);
    type matrix_t      is array (natural range <>, natural range <>) of integer;

    subtype quad_t is int_vector_t(0 to 3);
end package;

use work.issue1465_pkg.all;

entity issue1465 is
    generic (
        INTS  : int_vector_t;
        NATS  : nat_vector_t;
        REALS : real_vector_t;
        TIMES : time_vector_t;
        BITS  : bv_vector_t;
        QUAD  : quad_t );
end entity;

architecture test of issue1465 is
begin

    check: process(all) is
    begin
        -- The bounds of an unconstrained generic are taken from the value
        assert INTS'left = 0 severity failure;
        assert INTS'right = 3 severity failure;
        assert INTS'length = 4 severity failure;

        assert INTS = (-1, 0, 3, 4) severity failure;
        assert NATS = (0, 3, 2) severity failure;
        assert REALS = (1.5, -2.5) severity failure;
        assert TIMES = (10 ns, 1 us) severity failure;
        assert BITS = ("1010", "0101") severity failure;

        -- A constrained generic keeps the bounds of its subtype
        assert QUAD'left = 0 severity failure;
        assert QUAD'right = 3 severity failure;
        assert QUAD = (5, 6, 7, 8) severity failure;
    end process;

end architecture;

entity issue1465_matrix is
    generic ( M : work.issue1465_pkg.matrix_t );
end entity;

architecture test of issue1465_matrix is
begin
end architecture;
EOF

# Space around the elements and parenthesis is ignored
nvc -e --no-save issue1465 \
    -gINTS=' ( -1 , 0,3 ,4 ) ' \
    -gNATS='(0,3,2)' \
    -gREALS='(1.5,-2.5)' \
    -gTIMES='(10 ns,1 us)' \
    -gBITS='("1010","0101")' \
    -gQUAD='(5,6,7,8)' \
    -r

elab_fails() {
   local value="$1" expect="$2"
   if nvc -e --no-save issue1465 -gINTS="$value" -gNATS='(0)' -gREALS='(0.0)' \
          -gTIMES='(1 ns)' -gBITS='("0000")' -gQUAD='(1,2,3,4)' 2>err.txt; then
      echo "expected elaboration to fail for INTS=$value"
      exit 1
   fi
   grep -q "failed to parse \"$expect\" as type INT_VECTOR_T for generic INTS" \
        err.txt
}

elab_fails '(1,2,'  '(1,2,'
elab_fails '(1,2'   '(1,2'
elab_fails '(1,2))' '(1,2))'
elab_fails '()'     '()'
elab_fails '(,1)'   '(,1)'
elab_fails '(1,,2)' '(1,,2)'
elab_fails '1,2,3'  '1,2,3'

# The number of elements must match the bounds of a constrained generic
! nvc -e --no-save issue1465 \
    -gINTS='(1)' -gNATS='(0)' -gREALS='(0.0)' -gTIMES='(1 ns)' \
    -gBITS='("0000")' -gQUAD='(1,2,3)' 2>quad.txt
grep -q 'expected 4 elements for generic QUAD of type QUAD_T but have 3' \
     quad.txt

# Multidimensional arrays cannot be overridden
! nvc -e --no-save issue1465_matrix -gM='((1,2),(3,4))' 2>matrix.txt
grep -q 'cannot override generic M of type MATRIX_T' matrix.txt
