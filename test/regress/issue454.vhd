package pack is

    type rec is record
        x, y : integer;
    end record;

end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port (
        r : in rec );                   -- This signal gets optimised out in
                                        -- FST dump
end entity;

architecture test of sub is
begin
end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity issue454 is
end entity;

architecture test of issue454 is
    signal r : rec;
begin

    uut: entity work.sub port map ( r );

    main: process is
    begin
        r.x <= 5;
        wait for 1 ns;
        r.y <= 42;
        wait;
    end process;

end architecture;
