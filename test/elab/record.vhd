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

end architecture;

-------------------------------------------------------------------------------

entity elabr is
end entity;

use work.pack.all;

architecture test of elabr is
    signal r1, r2 : rec;
begin

    sub_i: entity work.sub
        port map (
            x => r1.a,
            y => r1.b,
            r => r2 );

end architecture;
