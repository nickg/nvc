use std.textio.all;
use std.env.all;

entity issue718 is
end entity;

architecture test of issue718 is
begin

    p1: process is
        variable l : line;
    begin
        for i in 1 to 10 loop
            write(l, string'("this is line "));
            write(l, i);
            writeline(output, l);
        end loop;

        stop;                           -- STOP message should appear after
                                        -- lines printed above
    end process;

end architecture;
