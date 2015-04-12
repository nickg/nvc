entity issue110 is
end entity;

use std.textio.all;

architecture test of issue110 is

    impure function write_func(x : integer) return integer is
        variable l : line;
    begin
        write(l, x);
        writeline(output, l);
        return x;
    end function;

    procedure write_proc(x : integer) is
        variable l : line;
    begin
        write(l, x);
        writeline(output, l);
    end procedure;

begin

    process is
    begin
        assert write_func(4) = 4;
        write_proc(5);
        wait;
    end process;

end architecture;
