package pack0 is
    type rec is record
        x : integer;
        y : bit_vector(1 to 3);
    end record;
end package;

-------------------------------------------------------------------------------

use work.pack0.all;

package pack1 is
    generic (x : rec := (55, "101"));
end package;

-------------------------------------------------------------------------------

package pack2 is
    generic (package p is new work.pack1 generic map (x => (12, "010")));
end package;

-------------------------------------------------------------------------------

package pack3 is
    generic (package p is new work.pack1 generic map (default));
end package;

-------------------------------------------------------------------------------

entity genpack4 is
end entity;

architecture test of genpack4 is

    package pack1_def is new work.pack1;
    package pack1_111 is new work.pack1 generic map ( x => (12, "111") );

    package p1 is new work.pack3 generic map ( pack1_def );  -- OK
    package p2 is new work.pack2 generic map ( pack1_111 );  -- Error

begin

end architecture;
