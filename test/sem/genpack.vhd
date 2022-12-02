package myfixed is
    generic ( whole : natural; frac : natural );  -- OK

    constant width : natural := whole + frac;  -- OK

    type fixed_t is array (1 to width) of bit;  -- OK

    function "+"(x, y : fixed_t) return fixed_t;  -- OK

end package;

package body myfixed is

    function "+"(x, y : fixed_t) return fixed_t is
        variable result : fixed_t;
    begin
        for i in 1 to width loop
            result(i) := x(i) or y(i);
        end loop;
        return result;
    end function;

end package body;

-------------------------------------------------------------------------------

package my_consts is
    constant x : integer := 5;
    constant y : integer := 6;
end package;

-------------------------------------------------------------------------------

use work.my_consts.all;

package myfixed_4_8 is new work.myfixed generic map ( x, y );  -- OK

-------------------------------------------------------------------------------

entity ent is
end entity;

architecture test of ent is
    package myfixed_2_4 is new work.myfixed generic map (2, 4);  -- OK
    package bad1 is new work.ent generic map (2, 4);  -- Error
    package bad2 is new std.standard generic map (2, 4);  -- Error
    package bad3 is new work.not_here generic map (2, 4);  -- Error
    package bad4 is new work.myfixed generic map (2);  -- Error

    constant c : natural := myfixed_2_4.width;  -- OK
    constant d : myfixed_2_4.fixed_t := (others => '0');  -- OK
begin

end architecture;

-------------------------------------------------------------------------------

package myfloat is
    generic ( package fixed_pkg is new work.myfixed generic map (<>) );  --  OK
    use fixed_pkg.width;                -- OK
end package;

package body myfloat is
    use fixed_pkg.all;                  -- OK
    constant k : natural := whole;      -- OK

    function add(a, b : fixed_t) return fixed_t is
    begin
        return a + b;                   -- OK
    end function;

end package body;

-------------------------------------------------------------------------------

package bad is
    generic ( package bad_std is new std.standard generic map (<>) );  -- Error
end package;

-------------------------------------------------------------------------------

package my_poly1 is new work.myfloat generic map ( fixed_pkg => not_here);  -- Error

-------------------------------------------------------------------------------

architecture test2 of ent is
    package p1 is new work.myfloat
                      generic map ( fixed_pkg => work.myfixed_4_8 );  -- OK

    package p2 is new work.myfloat
                      generic map ( fixed_pkg => p1 );  -- Error

    package p3 is new work.myfloat
                      generic map ( fixed_pkg => std.standard );  -- Error

    package p4 is new work.bad
          generic map ( bad_std => work.myfixed_4_8 );  -- Error (suppressed)
begin
end architecture;

-------------------------------------------------------------------------------

package ptr_pkg is
    generic ( type t );
    type ptr is access t;
end package;

-------------------------------------------------------------------------------

package int_ptr_pkg is new work.ptr_pkg
    generic map ( t => integer );

-------------------------------------------------------------------------------

use work.int_ptr_pkg;

architecture test3 of ent is
    procedure p is
        variable v : int_ptr_pkg.ptr;
    begin
        v := new integer;   -- OK
        int_ptr_pkg.deallocate(v);      -- OK
    end procedure;
begin
end architecture;
