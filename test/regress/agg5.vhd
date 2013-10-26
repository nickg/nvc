entity agg5 is
end entity;

architecture test of agg5 is
    signal vec  : bit_vector(4 downto 1);
    signal a, b : bit;
begin

    vec <= ( 1 => a, 2 => b, others => '0' );

    process is
    begin
        wait for 1 ns;
        assert vec = X"0";
        a <= '1';
        wait for 1 ns;
        assert vec = X"1";
        b <= '1';
        wait for 1 ns;
        assert vec = X"3";
        a <= '0';
        wait for 1 ns;
        assert vec = X"2";
        wait;
    end process;

end architecture;
