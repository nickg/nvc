entity sub is
    port (
        x : out integer := 5 );
end entity;

architecture test of sub is
begin

    p1: process is
    begin
        wait for 20 ns;
        x <= 10;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity driver4 is
end entity;

architecture test of driver4 is
    signal x : integer := 50;
begin

    sub_i: entity work.sub
        port map ( x => x );

    p2: process is
    begin
        report integer'image(x);
        assert x = 5;
        wait for 30 ns;
        assert x = 10;
        wait;
    end process;

end architecture;
