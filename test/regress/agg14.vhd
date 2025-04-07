entity agg14 is
end entity;

architecture test of agg14 is
    signal s, t : bit_vector(7 downto 0);
begin

    b: block is
        port ( i : in bit_vector; o : out bit_vector );
        port map ( s, t );

        type sample_reg is array (1 to 3) of bit_vector(i'range);
        signal samples : sample_reg;
    begin
        samples <= (i, samples(1 to 2));
        o <= samples(3);
    end block;

    check: process is
    begin
        s <= X"01";
        wait for 0 ns;
        s <= X"02";
        wait for 0 ns;
        s <= X"03";
        assert t = X"00";
        wait for 0 ns;
        assert t = X"00";
        wait for 0 ns;
        assert t = X"00";
        wait for 0 ns;
        assert t = X"01";
        wait for 0 ns;
        assert t = X"02";
        wait for 0 ns;
        assert t = X"03";
        wait;
    end process;

end architecture;
