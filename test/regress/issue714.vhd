entity sub is
    port ( p : in bit_vector );
end entity;

architecture test of sub is
begin

    process is
    begin
        assert p = "10";
        assert p'left = 0;
        assert p'right = 1;
        assert p'length = 2;
        wait for 5 ns;
        assert p = "11";
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity issue714 is
end entity;

architecture test of issue714 is
    signal x : bit;
begin

    u: entity work.sub
        port map (
            p(0) => '1',
            p(1) => x );

    x <= '1' after 4 ns;

end architecture;
