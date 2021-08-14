entity signal16 is
end entity;

architecture test of signal16 is
    signal x : bit_vector(15 downto 0);
    signal y : natural range 0 to 14;
    signal z : bit_vector(3 downto 0);
begin

    p1: process (y, z) is
    begin
        x(y+1 downto y) <= z(1 downto 0);
    end process;

    p2: process is
    begin
        assert x = "0000000000000000";
        y <= 14;
        z <= "1111";
        wait for 0 ns;
        assert x = "0000000000000000";
        y <= 1;
        wait for 0 ns;
        assert x = "1100000000000000";
        wait for 0 ns;
        assert x = "1100000000000110";
        wait;
    end process;

end architecture;
