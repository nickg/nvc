entity reflect3 is
end entity;

use std.reflection.all;

architecture test of reflect3 is
    type rec1 is record
        a, b : natural;
        c : string(1 to 3);
    end record;
begin

    p1: process is
        variable v1   : rec1 := (1, 2, "abc");
        variable vm   : value_mirror;
        variable rvm  : record_value_mirror;
        variable rstm : record_subtype_mirror;
        variable astm : array_subtype_mirror;
    begin
        vm := v1'reflect;
        rvm := vm.to_record;
        rstm := rvm.get_subtype_mirror;
        assert rstm.length = 3;
        assert rstm.element_name(0) = "A";
        assert rstm.element_name(1) = "B";
        assert rstm.element_name(2) = "C";
        assert rstm.element_index("B") = 1;
        assert rstm.element_subtype("A") = natural'reflect;
        assert rstm.element_subtype("C").get_type_class = class_array;
        assert rstm.to_subtype_mirror = rec1'reflect;     -- Should cache

        assert rvm.get(0).to_integer.value = 1;
        assert rvm.get("b").to_integer.value = 2;

        astm := rstm.element_subtype("C").to_array;
        assert astm.length = 3;

        assert rvm.get("c").to_array.get(1).to_enumeration.image = "'a'";
        wait;
    end process;

end architecture;
