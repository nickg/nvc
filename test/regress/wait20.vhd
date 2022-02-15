entity wait20 is
end entity;

architecture test of wait20 is
    signal x : bit_vector(1 to 3);
begin

    main: process is
    begin
        x <= "010" after 2 ns, "101" after 4 ns, "111" after 6 ns;
        wait on x(2) for 10 ns;          -- Splits the nexus
        assert now = 2 ns;
        assert x = "010";
        wait for 3 ns;
        assert x = "101";
        wait for 2 ns;
        assert x = "111";
        wait;
    end process;

end architecture;
