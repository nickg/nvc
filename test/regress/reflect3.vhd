entity reflect3 is
end entity;

use std.reflection.all;

architecture test of reflect3 is
    type rec1 is record
        a, b : integer;
    end record;
begin

    p1: process is
        variable v1   : rec1 := (1, 2);
        variable vm   : value_mirror;
        variable rvm  : record_value_mirror;
        variable rstm : record_subtype_mirror;
    begin
        vm := v1'reflect;
        rvm := vm.to_record;
        rstm := rvm.get_subtype_mirror;
        assert rstm.length = 2;
        assert rstm.element_name(0) = "A";
        assert rstm.element_name(1) = "B";
        assert rstm.element_index("B") = 1;
        assert rstm.element_subtype("A") = integer'reflect;
        assert rstm.to_subtype_mirror = rec1'reflect;     -- Should cache

        assert rvm.get(0).to_integer.value = 1;
        assert rvm.get("b").to_integer.value = 2;
        wait;
    end process;

end architecture;
