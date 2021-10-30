entity sub is
    port ( o1 : out integer;
           i1 : in real );
end entity;

architecture test of sub is
begin

    p1: process is
    begin
        o1 <= 1;
        wait for 1 ns;
        o1 <= 2;
        assert i1 = real(5);
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity conv3 is
end entity;

architecture test of conv3 is
    signal x : real;
    signal y : integer;
begin

    uut: entity work.sub
        port map ( real(o1) => x,
                   i1 => real(y) );

    p2: process is
    begin
        assert x = real(integer'left);
        wait for 0 ns;
        assert x = real(1);
        y <= 5;
        wait for 2 ns;
        assert x = real(2);
        wait;
    end process;

end architecture;
