package pkg1 is
    type my_int1 is range 1 to 10;
    type my_int2 is range 1 to 10;
    type my_int3 is range 1 to 10;
end package;

-------------------------------------------------------------------------------

package pkg2 is
    use work.pkg1.my_int2;              -- OK
    function func return my_int2;
end package;

package body pkg2 is
    use work.pkg1.my_int1;              -- OK
    constant x : my_int1 := 2;          -- OK

    function func return my_int2 is     -- OK
        use work.pkg1.my_int3;          -- OK
        variable y : my_int3;           -- OK
    begin
        return 0;
    end function;

    constant y : my_int3;               -- Error
end package body;

-------------------------------------------------------------------------------

entity ent is
    use work.pkg1.my_int1;              -- OK
    constant c : my_int1 := 2;          -- OK
    use work.spam;                      -- Error
    use work.1;                         -- Error
end entity;
