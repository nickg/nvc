package func is

    function sum(x, y, z : in integer) return integer;

    function invalid(x : out integer) return integer;  -- Error

    type uenum is (A, B, C);

    type uenum_vector is array (integer range <>) of uenum;
    
    function resolved(v : uenum_vector) return uenum;

    subtype enum is resolved uenum;

    subtype enum_ab is resolved uenum range A to B;

    function resolved2(v : uenum) return uenum;

    subtype enum_bad1 is resolved2 uenum;  -- Error

    function resolved3(v : uenum; x : integer) return uenum;

    subtype enum_bad2 is resolved3 uenum;  -- Error

    subtype enum_bad3 is uenum uenum;   -- Error

    function default(x : in uenum := 6) return uenum;  -- Error
    
end package;

package body bad is                     -- Error
end package body;

package body func is

    function sum(x, y, z : in integer) return integer is
    begin
        return x + y;                       -- OK
    end function;

    function test1(x : integer) return integer is
    begin
        return A;                       -- Wrong return type
    end function;

    function test2(x : out integer) return integer is  -- Invalid mode
    begin
        return 0;
    end function;

    function test3(x : integer) return integer is
    begin
        null;         -- Missing return statement
    end function;

    function foo(x, y, z : in integer) return integer;
    function foo(x, y, z : in integer) return integer;  -- Duplicate

    function test4(x : uenum_vector) return uenum is
    begin
        return x(x'low);
    end function;
    
end package body;
