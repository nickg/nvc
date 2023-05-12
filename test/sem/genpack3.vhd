package pack1 is
    generic (x : integer := 5; y : integer := 2);
end package;

-------------------------------------------------------------------------------

package pack2 is
    generic (x : integer; y : integer := 2);
end package;

-------------------------------------------------------------------------------

package pack3 is
    component comp1 is
        generic (
            package p is new work.pack1 generic map (x => 2) );  -- OK
    end component;

    component comp2 is
        generic (
            package p is new work.pack1 generic map (z => 2) );  -- Error
    end component;

    component comp3 is
        generic (
            package p is new work.pack1 generic map (default) );  -- OK
    end component;

    component comp4 is
        generic (
            package p is new work.pack2 generic map (default) );  -- Error
    end component;
end package;
