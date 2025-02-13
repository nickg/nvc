entity stdenv3 is
end entity;

use std.env.all;

architecture test of stdenv3 is

    procedure proc1 is
        variable stack : call_path_vector_ptr;
    begin
        stack := get_call_path;
        assert stack'length = 4;
        report to_string(stack);
    end procedure;

    procedure proc2 is
    begin
        proc1;
    end procedure;

    procedure proc3 is
    begin
        proc2;

        assert file_name = "stdenv3.vhd" report file_name;
        report file_path;
        assert file_line = 27;
        assert file_line = "28";
    end procedure;

begin

    p1: process is
        variable stack : call_path_vector_ptr;

        function ends_with(x, y : string) return boolean is
        begin
            return x'length >= y'length and x(x'right - y'length + 1 to x'right) = y;
        end function;
    begin
        stack := get_call_path;
        assert stack'length = 1;
        assert stack(0).name.all = "P1";
        assert stack(0).file_name.all = "stdenv3.vhd";
        assert stack(0).file_line = 41;

        proc3;

        report file_path;
        assert not ends_with(file_path, "stdenv3.vhd");
        wait;
    end process;

end architecture;
