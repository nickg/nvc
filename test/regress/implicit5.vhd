entity implicit5 is
end entity;

architecture test of implicit5 is
    signal x : bit_vector(1 to 3);
begin

    p1: process is
    begin
        x <= "101";
        assert x'delayed = "000";
        wait for 0 ns;
        assert x'delayed = "000";
        wait for 0 ns;
        assert x'delayed = "101";
        x(2) <= '1';
        wait for 0 ns;
        assert x'delayed = "101";
        wait for 0 ns;
        assert x'delayed = "111";
        wait for 1 ns;

        x <= "000" after 1 ns, "111" after 2 ns, "101" after 3 ns;

        assert x'delayed(5 ns) = "000";
        wait for 5 ns;
        assert x'delayed(5 ns) = "111";
        wait for 1 ns;
        assert x'delayed(5 ns) = "000";
        wait for 1 ns;
        assert x'delayed(5 ns) = "111";
        wait for 1 ns;
        assert x'delayed(5 ns) = "101";

        wait;
    end process;

end architecture;
