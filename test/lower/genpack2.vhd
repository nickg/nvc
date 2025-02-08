package pack1 is
    type t_rec is record
        x, y : integer;
    end record;
end package;

-------------------------------------------------------------------------------

use work.pack1.all;

package genpack is
    generic ( g : t_rec );
end package;

-------------------------------------------------------------------------------

package test is
    package inst is new work.genpack generic map ( g => (1, 2) );
    use inst.all;
    constant c : integer := g.y;
end package;
