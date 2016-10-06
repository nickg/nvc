package p is
    signal s : bit;
end package;

entity sub is
    port (
        i : in bit );
end entity;

architecture test of sub is
begin
    assert i'last_value = '1';
end architecture;

entity tag is
end entity;

use work.p.all;

architecture test of tag is
    signal p, x, y : bit;
begin

    process (x) is
    begin
        assert x'last_value = '1';
    end process;

    process (p) is
    begin
        assert p = '1';
    end process;

    sub_i: entity work.sub port map ( y );

end architecture;
