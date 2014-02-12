entity stop1 is
end entity;

library nvc;
use nvc.env.all;

architecture test of stop1 is
begin

    process is
    begin
        stop(0);
        report "should not print this" severity failure;
        wait;
    end process;

end architecture;
