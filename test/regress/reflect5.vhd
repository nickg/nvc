entity reflect5 is
end entity;

use std.reflection.all;
use std.textio.all;

architecture test of reflect5 is
    function basename(path : string) return string is
    begin
        for i in path'reverse_range loop
            if path(i) = '\' or path(i) = '/' then
                return path(i + 1 to path'length);
            end if;
        end loop;
        return path;
    end function;

    type pt is protected
        procedure proc;
    end protected;

    type pt is protected body
        procedure proc is
        begin
        end procedure;
    end protected body;
begin

    p1: process is
        variable stm  : subtype_mirror;
        variable fstm : file_subtype_mirror;
        variable astm : access_subtype_mirror;
        variable fvm  : file_value_mirror;
        variable avm  : access_value_mirror;
        variable pstm : physical_subtype_mirror;
        variable pvm  : physical_value_mirror;
        file f : text;
        variable ptr : line;
        variable d : delay_length;
        variable p : pt;
    begin
        stm := text'reflect;
        assert stm.get_type_class = CLASS_FILE;
        fstm := stm.to_file;
        assert fstm.designated_subtype.simple_name = "STRING";

        file_open(f, "temp.txt", WRITE_MODE);

        fvm := f'reflect.to_file;
        assert fvm.get_file_open_kind = WRITE_MODE;
        assert basename(fvm.get_file_logical_name) = "temp.txt";

        stm := line'reflect;
        assert stm.get_type_class = CLASS_ACCESS;
        astm := stm.to_access;
        assert astm.designated_subtype.simple_name = "STRING";

        avm := ptr'reflect.to_access;
        assert avm.is_null;

        ptr := new string'("hello");
        avm := ptr'reflect.to_access;
        assert not avm.is_null;
        assert avm.get.to_array.get_subtype_mirror.length = 5;
        assert avm.get.to_array.get(1).to_enumeration.image= "'h'";
        assert avm.get.to_array.get(5).to_enumeration.image= "'o'";

        stm := time'reflect;
        assert stm.get_type_class = CLASS_PHYSICAL;
        pstm := stm.to_physical;
        assert pstm.units_length = 8;
        assert pstm.unit_index("ns") = 3;
        assert pstm.unit_name(5) = "MS";
        assert pstm.scale("fs") = 1;
        assert pstm.scale("ns") = 1000000;

        d := 20 ns;
        pvm := d'reflect.to_physical;
        assert pvm.value = 20000000;
        assert pvm.image = "20000000 FS";

        stm := pt'reflect;
        assert stm.get_type_class = CLASS_PROTECTED;
        assert stm.to_protected.to_subtype_mirror = stm;

        assert p'reflect.get_value_class = CLASS_PROTECTED;

        wait;
    end process;

end architecture;
