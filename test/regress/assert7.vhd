entity assert7 is
end entity;

use std.env.all;

architecture test of assert7 is
begin

    p1: process is
    begin
        for i in 1 to 30 loop
            assert false report "this is error " & integer'image(i);
        end loop;
        finish;                      -- Should preserve exit status
        wait;
    end process;

end architecture;
