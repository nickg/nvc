package pack1 is
    generic (x : integer := 5; y : integer := 2; z : bit_vector(1 to 3) := "101");
end package;

-------------------------------------------------------------------------------

package pack2 is
    generic (package p is new work.pack1 generic map (x => 2));
end package;

-------------------------------------------------------------------------------

package pack3 is
    generic (package p is new work.pack1 generic map (default));
end package;

-------------------------------------------------------------------------------

entity genpack2 is
end entity;

architecture test of genpack2 is

    package pack1_def is new work.pack1;
    package pack1_55 is new work.pack1 generic map ( 5, 5 );

    package p1 is new work.pack3 generic map ( pack1_def );  -- OK
    package p2 is new work.pack3 generic map ( pack1_55 );  -- Error

begin

end architecture;
