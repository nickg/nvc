package components is
    component sub is
        port (
            x : in real;
            z : out real );
    end component;
end package;

-------------------------------------------------------------------------------

entity top is
end entity;

use work.components.all;

architecture test of top is
    signal xr, zr : real := 0.0;
begin

    u1: sub port map ( xr, zr );

end architecture;
