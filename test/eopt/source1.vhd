entity sub is
    port (
        i : in bit;
        o : out bit );
end entity;

architecture test of sub is
begin
    o <= i;
end architecture;

-------------------------------------------------------------------------------

entity source1 is
end entity;

architecture test of source1 is
    signal x : bit;
    signal y : bit_vector(1 to 5);
begin

    x <= '1';
    foo: x <= '0';                           -- Error

    y <= "10000";
    y(2 to 3) <= "11";                  -- Error

    sub1_i: entity work.sub port map ( x, y(4) );
    sub2_i: entity work.sub port map ( x, y(2) );

end architecture;
