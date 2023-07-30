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
begin

    p1: process is
        variable stm  : subtype_mirror;
        variable fstm : file_subtype_mirror;
        variable fvm  : file_value_mirror;
        file f : text;
    begin
        stm := text'reflect;
        assert stm.get_type_class = CLASS_FILE;
        fstm := stm.to_file;
        assert fstm.designated_subtype.simple_name = "STRING";

        file_open(f, "temp.txt", WRITE_MODE);

        fvm := f'reflect.to_file;
        assert fvm.get_file_open_kind = WRITE_MODE;
        assert basename(fvm.get_file_logical_name) = "temp.txt";

        wait;
    end process;

end architecture;
