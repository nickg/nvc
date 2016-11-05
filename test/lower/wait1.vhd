-- A very basic sanity test of wait statements
entity wait1 is
end entity;

architecture test of wait1 is
begin

    p1: process is
    begin
        assert now = 0 ns;
        wait_1: wait for 1 ns;
        assert now = 1 ns;
        wait for 1 fs;
        assert now = 1000001 fs;
        end_wait: wait;
    end process;

end architecture;
