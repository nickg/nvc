package pack is

    type rec is record
        a, b : integer;
    end record;

end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port (
        x : in  integer;
        y : out integer;
        r : in  rec );
end entity;

architecture test of sub is
begin

   y <= x + r.a + r.b;

end architecture;

-------------------------------------------------------------------------------

entity elab21 is
end entity;

use work.pack.all;

architecture test of elab21 is
    signal r1, r2 : rec := (0, 0);
begin

    sub_i: entity work.sub
        port map (
            x => r1.a,
            y => r1.b,
            r => r2 );

    process is
    begin
        r1.a <= 0;
        r2 <= (0, 0);
        wait for 1 ns;
        assert r1.b = 0;
        r1.a <= 5;
        wait for 1 ns;
        assert r1.b = 5;
        r2 <= (2, 3);
        wait for 1 ns;
        assert r1.b = 10;
        wait;
    end process;

end architecture;
