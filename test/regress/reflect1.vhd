entity reflect1 is
end entity;

use std.reflection.all;

architecture test of reflect1 is
begin

    p1: process is
        variable v1   : integer := 42;
        variable v2   : integer_vector(1 to 3) := (1, 2, 3);
        variable v3   : real := 1.234;
        variable vm   : value_mirror;
        variable ivm  : integer_value_mirror;
        variable avm  : array_value_mirror;
        variable fvm  : floating_value_mirror;
        variable evm  : enumeration_value_mirror;
        variable stm  : subtype_mirror;
        variable istm : integer_subtype_mirror;
        variable astm : array_subtype_mirror;
        variable fstm : floating_subtype_mirror;
        variable estm : enumeration_subtype_mirror;
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
        assert ivm.get_subtype_mirror = istm;
        assert istm.ascending;
        assert istm.length = 0;         -- XXX: should be an error
        assert istm.low.value = integer'left;
        assert istm.high.value = integer'right;
        assert istm.left = istm.low;
        assert istm.right = istm.high;

        assert integer'reflect = stm;

        vm := v2'reflect;
        assert vm.get_value_class = CLASS_ARRAY;
        avm := vm.to_array;
        assert avm.get(1).to_integer.value = 1;
        assert avm.get(2).to_integer.value = 2;
        assert avm.get((1 => 3)).to_integer.value = 3;
        astm := avm.get_subtype_mirror;
        assert astm.dimensions = 1;
        assert astm.index_subtype = natural'reflect;
        assert astm.element_subtype = integer'reflect;
        assert astm.length = 3;
        assert astm.left = 1;
        assert astm.right = 3;
        assert astm.ascending;
        assert astm.low = 1;
        assert astm.high = 3;

        fvm := v3'reflect.to_floating;
        assert fvm.value = 1.234;
        assert fvm.get_subtype_mirror.to_subtype_mirror = real'reflect;

        evm := true'reflect.to_enumeration;
        assert evm.pos = 1;
        assert evm.image = "true";
        estm := evm.get_subtype_mirror;
        assert estm.length = 2;
        assert estm.ascending;
        assert estm.left.image = "false";
        assert estm.right.pos = 1;
        assert character'reflect.to_enumeration.enumeration_literal(72).image = "'H'";
        assert estm.enumeration_literal("true").pos = 1;

        wait;
    end process;

end architecture;
