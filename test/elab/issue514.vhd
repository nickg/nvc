package pack is
    type params is record
        x, y : integer;
    end record;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    generic ( p : params );
end entity;

architecture test of sub is
    constant px : integer := p.x;
    constant py : integer := p.y;
begin

    g: for i in px to py generate
    end generate;

end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity issue514 is
end entity;

architecture test of issue514 is
    function get_params return params is
    begin
        return (x => 4, y => 6);
    end function;
begin

    u: entity work.sub
        generic map (p => get_params);

end architecture;
