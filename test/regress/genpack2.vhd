package poly is
    generic (a, b : integer);
    function apply (x : integer) return integer;
end package;

package body poly is
    function apply (x : integer) return integer is
    begin
        return x * a + b;
    end function;
end package body;

-------------------------------------------------------------------------------

package wrapper is
    generic ( package p is new work.poly generic map ( <> ) );
    function wrapped_apply (n : integer) return integer;
end package;

package body wrapper is
    use p.all;

    function wrapped_apply (n : integer) return integer is
    begin
        return apply(n);
    end function;
end package body;

-------------------------------------------------------------------------------

entity genpack2 is
end entity;

architecture test of genpack2 is
    package my_poly1 is new work.poly generic map (a => 2, b => 3);
    package my_wrap1 is new work.wrapper generic map (p => my_poly1);
begin

    main: process is
        variable v : integer := 5;
    begin
        assert my_wrap1.wrapped_apply(2) = 7;
        wait for 1 ns;
        assert my_wrap1.wrapped_apply(v) = 13;
        wait;
    end process;

end architecture;
