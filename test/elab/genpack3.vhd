package pack1 is
    generic (x : integer := 5; y : integer := 2; z : bit_vector(1 to 3) := "101");
end package;

-------------------------------------------------------------------------------

package pack2 is
    generic (package p is new work.pack1 generic map (x => 5, z => "100"));
end package;

-------------------------------------------------------------------------------

package pack3 is
    generic (package p is new work.pack1 generic map (default));
end package;

-------------------------------------------------------------------------------

entity genpack3 is
end entity;

architecture test of genpack3 is

    package pack1_def is new work.pack1;
    package pack1_111 is new work.pack1 generic map ( z => "111" );

    package p2 is new work.pack2 generic map ( pack1_111 );  -- Error

begin

end architecture;
