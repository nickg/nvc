entity stop2 is
end entity;

library std;
use std.env.all;

architecture test of stop2 is
begin

    process is
    begin
        stop(0);
        report "should not print this" severity failure;
        wait;
    end process;

end architecture;
