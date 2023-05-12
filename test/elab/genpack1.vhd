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

entity genpack1 is
end entity;

architecture test of genpack1 is

    package pack1_def is new work.pack1;

    package p1 is new work.pack2 generic map ( pack1_def );

begin

end architecture;
