package pack is
    type t_rec is record
        x : integer;
        y : string;
    end record;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity vhpi11 is
    generic (
        g0 : t_rec := (55, "hello") );
end entity;

architecture test of vhpi11 is
begin
end architecture;
