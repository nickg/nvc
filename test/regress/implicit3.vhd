entity sub is
    port (
        x : in integer;
        y : out boolean );
end entity;

architecture test of sub is
begin

    y <= x'delayed(5 ns) > x;

end architecture;

-------------------------------------------------------------------------------

entity implicit3 is
end entity;

architecture test of implicit3 is
    signal x : integer := 0;
    signal y : boolean;
begin

    sub_i: entity work.sub
        port map (x, y);

    process is
    begin
        x <= 1;
        wait for 1 ns;
        assert not y;
        wait for 5 ns;
        assert not y;
        x <= -1;
        wait for 5 ns;
        assert y;
        wait;
    end process;


end architecture;
