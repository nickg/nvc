package ename_pack is
    constant c : integer := << constant .top.foo : integer >>;  -- Not supported
end package;

-------------------------------------------------------------------------------

entity ename is
end entity;

architecture test of ename is
    alias e1 is <<signal .top.foo.bar : bit>>;  -- OK
    constant k1 : integer := <<constant foo.bar : integer>>;  -- OK
    signal s1 : bit_vector(1 to k1);    -- OK
    signal s2 : bit_vector(1 to <<signal foo.bar : integer>>);    -- Error
    constant k2 : integer := <<constant foo.baz : bit>>;  -- Error
begin

    p1: process is
    begin
        e1 <= '1';                      -- OK
        e1 := '1';                      -- Error
        <<variable foo.var : integer>> := 5;  -- OK
        <<signal ^.x.y : bit>> <= force '1';  -- OK
        <<constant .x.y : bit>> <= release;  -- Error
        assert <<constant foo(<< signal a.b : integer >>).bar : integer>> = 1;  -- Error
        wait;
    end process;

end architecture;
