entity block1 is
end entity;

architecture test of block1 is
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

end architecture;
