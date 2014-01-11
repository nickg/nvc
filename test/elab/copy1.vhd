entity sub is
    port (
        x : in integer;
        y : out integer );
end entity;

architecture test of sub is

    function double(n : integer) return integer is
        type blah is range 1 to 3;
        variable r : integer;
    begin
        r := n * 2;
        return r;
    end function;

    shared variable global : integer;

begin

    y <= double(x);

    process (x)
    begin
        global := x;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity copy1 is
end entity;

architecture test of copy1 is
    signal x1, y1, x2, y2 : integer;
begin

    sub1_i: entity work.sub
        port map ( x1, y1 );

    sub2_i: entity work.sub
        port map ( x2, y2 );

end architecture;
