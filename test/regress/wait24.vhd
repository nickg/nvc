library nvc;
use nvc.sim_pkg.all;

entity wait24 is
end entity;

architecture test of wait24 is
begin

    p1: process is
    begin
        assert now = 0 ns;
        assert current_delta_cycle = 0;
        wait for 0 ns;
        assert now = 0 ns;
        assert current_delta_cycle = 1;
        for i in 1 to 5 loop
            wait for 0 ns;
        end loop;
        assert current_delta_cycle = 6;
        wait for 1 ps;
        assert current_delta_cycle = 0;
        wait;
    end process;

end architecture;
