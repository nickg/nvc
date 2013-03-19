entity bitand is
    port (
        x, y : in bit;
        z : out bit );
end entity;

architecture test of bitand is
begin
    z <= x and y;
end architecture;

entity issue9 is
end entity;

architecture test of issue9 is
    signal x1, y1, z1 : bit;
begin
    bitand_i: entity work.bitand
        port map (
            x=>x1,
            y=>'0', --y=>y1 works !
            z=>z1 );
    process is
    begin
        y1 <='1';
        x1 <= '0';
        wait for 1 ns;
        assert z1 = '0';
        x1<='1';
        wait for 1 ns;
        assert z1 = '0';
        wait;
    end process;
end architecture;
