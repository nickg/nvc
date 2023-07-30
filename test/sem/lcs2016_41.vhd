entity lcs2016_41 is
end entity;

use std.reflection.all;

architecture test of lcs2016_41 is
begin

    p1: process is
        variable stm : subtype_mirror;
        variable astm : array_subtype_mirror;
        variable vm : value_mirror;
        variable i : integer;
        variable v : bit_vector(1 to 3);
        type int_ptr_t is access integer;
        variable p : int_ptr_t;
    begin
        stm := integer'reflect;         -- OK
        vm := i'reflect;                -- OK
        i := integer'reflect;           -- Error
        i := i'reflect;                 -- Error
        stm := v'subtype'reflect;       -- OK
        assert stm.get_type_class = CLASS_ARRAY;  -- OK
        astm := stm.to_array;           -- OK
        vm := p1'reflect;               -- Error
        assert v'subtype'reflect.get_type_class = CLASS_ARRAY;  -- OK
        stm := bit_vector'reflect;      -- Error
        stm := int_ptr_t'reflect;       -- OK
        wait;
    end process;

end architecture;
