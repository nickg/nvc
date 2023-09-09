package pack is
    constant k : string;
end package;

package body pack is
    constant k : string := "hello, world";
end package body;

-------------------------------------------------------------------------------

entity sub is
end entity;

architecture test of sub is
    constant c : integer := work.pack.k'length;
begin
end architecture;

-------------------------------------------------------------------------------

entity link1 is
end entity;

architecture test of link1 is
    component sub is
    end component;
begin
    u: component sub;
end architecture;
