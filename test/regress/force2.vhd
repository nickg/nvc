entity force2 is
end entity;

architecture test of force2 is
    signal s : bit_vector(1 to 3);
begin

    p1: process is
        variable n : integer;
    begin
        n := 3;
        assert s = "000";
        s <= "101";
        wait for 1 ns;
        s <= force "010";
        wait for 0 ns;
        assert s = "010";
        s(n) <= '0';
        wait for 0 ns;
        assert s = "010";
        s <= release;
        wait for 0 ns;
        assert s = "100";
        wait;
    end process;

end architecture;
