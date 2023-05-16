package pack is
    type pt is protected
        procedure do_something;
    end protected;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port ( variable sv : inout pt );    -- OK
end entity;

architecture test of sub is
begin
    p1: process is
    begin
        sv.do_something;                -- OK
        wait;
    end process;
end architecture;

-------------------------------------------------------------------------------

entity lcs2016_47 is
end entity;

use work.pack.all;

architecture test of lcs2016_47 is
    shared variable sv : pt;

    component c1 is
        port ( variable x : out pt;   -- Error
               variable y : inout integer );  -- Error
    end component;

    signal i : integer;
begin

    u1: entity work.sub port map ( sv );  -- OK

    u2: component c1 port map ( sv, i );  --Error

end architecture;
