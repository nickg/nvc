package pack is
    component foo is
    end component;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity top is
end entity;

architecture test of top is
begin
    u1: foo;                            -- OK
end architecture;
