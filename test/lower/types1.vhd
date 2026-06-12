package util is
    impure function expensive return integer;
end package;

-------------------------------------------------------------------------------

use work.util.all;

package types1_def is
    subtype my_int1 is integer range 1 to expensive;
    subtype my_array1 is bit_vector(1 to expensive);
end package;

use work.types1_def.all;

package types1_use is
    constant c1 : my_int1 := 5;
end package;
