-- Test shared variables in -2008 with --relax=shared
entity shared4 is
end entity;

architecture test of shared4 is
    shared variable x : natural;
begin

    g0: for i in 1 to 10 generate
    begin
        p1: process is
        begin
            for i in 1 to 1000 loop
                x := x + 1;                 -- Racy!
            end loop;
            wait;
        end process;
    end generate;

    p2: process is
    begin
        wait for 1 ns;
        assert x = 10 * 1000;           -- May fail multithreaded sim
        wait;
    end process;

end architecture;
