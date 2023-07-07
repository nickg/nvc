entity reflect2 is
end entity;

use std.reflection.all;

architecture test of reflect2 is

    procedure test_int (constant l, r : in integer) is
        type my_int is range l to r;
        variable v1   : my_int := 7;
        variable vm   : value_mirror;
        variable ivm  : integer_value_mirror;
        variable stm  : subtype_mirror;
        variable istm : integer_subtype_mirror;
    begin
        vm := v1'reflect;
        assert vm.get_value_class = CLASS_INTEGER;
        ivm := vm.to_integer;
        assert ivm.value = 7;
        v1 := 0;
        wait for 1 ns;
        assert ivm.value = 7;
        assert ivm.image = "7";
        assert v1'reflect.all.to_integer.value = 0;
        assert ivm.to_value_mirror = vm;

        stm := vm.get_subtype_mirror;
        assert stm.get_type_class = CLASS_INTEGER;
        istm := stm.to_integer;
        assert istm.simple_name = "MY_INT";
        assert istm.to_subtype_mirror = stm;

        -- These two are both incorrect, just documenting the behaviour
        assert istm.left.value = integer'left;
        assert istm.right.value = integer'right;
    end procedure;

    procedure test_array (constant l, r : in integer) is
        variable a : integer_vector(l to r);
        variable vm   : value_mirror;
        variable stm  : subtype_mirror;
        variable astm : array_subtype_mirror;
    begin
        vm := a'reflect;
        stm := vm.get_subtype_mirror;
        astm := stm.to_array;
        assert astm.left = index(l);
        assert astm.right = index(r);
        assert astm.length = index(maximum(r - l + 1, 0));
    end procedure;

begin

    p1: process is
        type small_int is range 1 to 10;
        type small_real is range 0.0 to 1.0;
    begin
        assert small_int'reflect.to_integer.right.value = 10;
        assert small_real'reflect.to_floating.left.value = 0.0;
        test_int(-10, 10);
        test_array(1, 10);
        test_array(2, 3);
        wait;
    end process;

end architecture;
