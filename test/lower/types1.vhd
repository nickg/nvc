package util is
    impure function expensive return integer;
end package;

-------------------------------------------------------------------------------

use work.util.all;

package types1 is
    subtype my_int1 is integer range 1 to expensive;
    constant c1 : my_int1 := 5;
end package;
