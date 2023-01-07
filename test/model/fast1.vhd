entity fast1 is
end entity;

architecture test of fast1 is
    signal x : natural;
begin

    p1: process is
    begin
        x <= 1;
        wait for 0 ns;
        x <= 2 after 1 ns;
        wait;
    end process;

end architecture;
