package pack is
    type rec is record
        x : integer;
        y : bit;
    end record;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port (
        r : in rec;
        x : out integer;
        y : out bit );
end entity;

architecture test of sub is
begin
    x <= r.x;
    y <= r.y;
end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity record15 is
end entity;

architecture test of record15 is
    signal x : integer;
    signal y : bit;
begin

    sub_i: entity work.sub
        port map (
            r => (123, '1'),
            x => x,
            y => y );

    process is
    begin
        wait for 1 ns;
        assert x = 123;
        assert y = '1';
        wait;
    end process;

end architecture;
