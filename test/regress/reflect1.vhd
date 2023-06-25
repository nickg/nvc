entity reflect1 is
end entity;

use std.reflection.all;

architecture test of reflect1 is
begin

    p1: process is
        variable v1   : integer := 42;
        variable vm   : value_mirror;
        variable ivm  : integer_value_mirror;
        variable stm  : subtype_mirror;
        variable istm : integer_subtype_mirror;
    begin
        vm := v1'reflect;
        assert vm.get_value_class = CLASS_INTEGER;
        ivm := vm.to_integer;
        assert ivm.value = 42;
        v1 := 100;
        wait for 1 ns;
        assert ivm.value = 42;
        assert ivm.image = "42";
        assert v1'reflect.all.to_integer.value = 100;
        assert ivm.to_value_mirror = vm;

        stm := vm.get_subtype_mirror;
        assert stm.get_type_class = CLASS_INTEGER;
        istm := stm.to_integer;
        assert istm.simple_name = "INTEGER";
        assert istm.to_subtype_mirror = stm;

        wait;
    end process;

end architecture;
