entity block2 is
end entity;

architecture test of block2 is
    signal x, y : integer;
begin

    b1: block is
        generic ( g1 : integer );
        generic map ( g1 => 5 );
        port ( i : in integer; o : out integer );
        port map ( i => x, o => y );
    begin
        o <= i + g1;
    end block;

    stim: process is
    begin
        x <= 1;
        wait for 1 ns;
        assert y = 6;
        x <= 10;
        wait for 1 ns;
        assert y = 15;
        wait;
    end process;

end architecture;
