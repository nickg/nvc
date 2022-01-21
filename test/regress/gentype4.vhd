package cmp_pkg is
    generic ( type t );

    function equal (a, b : t) return boolean;
end package;

package body cmp_pkg is

    function equal (a, b : t) return boolean is
    begin
        return a = b;
    end function;

end package body;

-------------------------------------------------------------------------------

entity gentype4 is
end entity;

architecture test of gentype4 is
    package cmp_int is new work.cmp_pkg generic map ( integer );
    use cmp_int.all;
begin

    main: process is
        variable v : integer;
    begin
        v := 4;
        wait for 1 ns;
        assert equal(4, v);
        assert not equal(5, 6);
        wait;
    end process;

end architecture;
