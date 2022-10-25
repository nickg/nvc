entity sub is
    port ( c : out integer := 0;
           a, b : in integer );
end entity;

architecture impl of sub is
begin
    c <= a + b;
end architecture;

-------------------------------------------------------------------------------

entity elab27 is
end entity;

architecture test of elab27 is

    component comp is
        port ( b : in integer;
               c : out integer;
               a : in integer );
    end component;

    signal x, y1, y2, z : integer := 0;

    for all : comp use entity work.sub;

    function negate (x : in integer) return integer is
    begin
        return -x;
    end function;
begin

    uut1: component comp port map (x, y1, z);

    uut2: component comp
        port map (x, negate(c) => y2, a => z);

    p1: process is
    begin
        x <= 1;
        z <= 2;
        wait for 1 ns;
        assert y1 = 3;
        assert y2 = -3;
        wait;
    end process;

end architecture;
