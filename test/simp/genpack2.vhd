package pack1 is
    generic (x, y : integer);
    constant c : integer := x + y;
end package;

-------------------------------------------------------------------------------

package pack2 is
    generic (package p is new work.pack1 generic map (<>));
end package;

-------------------------------------------------------------------------------

package pack3 is
    generic (package p is new work.pack2 generic map (<>));  -- Should not simplify
    package pi is new work.pack1 generic map (1, 2);  -- Should fold C
end package;
