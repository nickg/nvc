package cmp_pkg is
    generic ( type t;
              function cmp_fn (x, y : t) return boolean );

    function equal (a, b : t) return boolean;
end package;

package body cmp_pkg is

    function equal (a, b : t) return boolean is
    begin
        return cmp_fn(a, b);
    end function;

end package body;

-------------------------------------------------------------------------------

entity gentype2 is
end entity;

architecture test of gentype2 is
    package cmp_int is new work.cmp_pkg
            generic map ( t => integer, cmp_fn => "=" );
    use cmp_int.all;
begin

    main: process is
    begin
        assert equal(4, 4);
        assert not equal(5, 6);
        wait;
    end process;

end architecture;
