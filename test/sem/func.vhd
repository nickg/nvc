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

    function default1(x : in uenum := 6) return uenum;  -- Error

    function foo return integer is      -- Error
    begin
        return 4;
    end function;

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

    function test5(x, y : uenum) return uenum is
        type uenum2d is array (uenum, uenum) of uenum;
        constant table : uenum2d :=
            ( ( A, A, A ),
              ( A, B, C ),
              ( A, C, B ) );
    begin
        return table(x, y);
    end function;

    function test6(x : uenum_vector) return uenum_vector is
        variable tmp : uenum_vector(1 to x'length);
    begin
        for i in tmp'range loop
            tmp(i) := A;
        end loop;
        return tmp;
    end function;

    function test7(x : uenum_vector) return uenum_vector is
        subtype rtype is uenum_vector(x'length downto 0);
        variable r : rtype;
    begin
        return r;
    end function;

    function test8(x : uenum) return uenum_vector is
    begin
        return test7((1 to 3 => x));
    end function;

    function default2(y : in integer := 6) return integer is
    begin
        return y * 2;
    end function;

    function test9 return integer is
    begin
        return default2;
    end function;

    function test10(k : in integer) return integer is
        variable v : integer;
        variable u : uenum;
    begin
        v := sum(x => 4, 1, 2);         -- Error
        v := sum(1, x => 4, x => 4);    -- Error
        v := sum(1, y => k, z => 4);    -- OK
        u := resolved3(A, x => 4);      -- OK
        u := resolved3(x => 3, v => B);  -- OK
        return v;
    end function;

    function test11(constant c : in bit) return bit;  -- OK

    function test12(variable v : in bit) return bit;  -- Error

    type ft is file of bit;

    function test13(file f : ft) return bit;  -- OK

    function test14(signal s : bit) return bit;  -- OK

    procedure modify(variable b : inout bit) is
    begin
        b := '1';
    end procedure;

    function test15(file f : ft) return bit is
        variable b : bit;
    begin
        read(f, b);                     -- OK
        return b;
    end function;

    function test16(x : in bit) return bit is
    begin
        modify(x);  -- Error
        return x;
    end function;

    impure function test17(x : in bit) return bit is
    begin
        if now = 10 ns then
            return '1';
        else
            return '0';
        end if;
    end function;

    function test18(x : in bit) return bit is
    begin
        return not test17(x);           -- Error, test18 not impure
    end function;

    type int_ptr is access integer;

    function test19(x : in int_ptr) return integer;  -- Error

    function recur(x : in integer) return integer is
    begin
        if x = 0 then
            return 1;
        else
            return x * recur(x - 1);
        end if;
    end function;

    function test20(x : integer := 5; y : real) return integer is
        variable k : integer;
    begin
        k := test20(6.5);               -- Error
        k := test20(5);                 -- Error
        k := test20(y => 7);            -- Error
        return k;
    end function;

    function test21a(x : string) return integer;
    function test21a(x : bit_vector) return integer;

    function test21 return integer is
    begin
        return test21a(';' & LF);       -- OK
    end function;

    function test22a(x : integer) return integer is
    begin
        return x + 1;
    end function;

    function test22a(x : integer) return real is
    begin
        return real(x) + 1.0;
    end function;

    function test22 return integer is
    begin
        assert test22a(1) = 2;          -- OK
        assert test22a(1) = 2.0;        -- OK
        return 1;
    end function;

    impure function test23 return integer is
        variable x : integer;

        impure function sub(y : in integer) return integer is
        begin
            return x + y;
        end function;
    begin
        x := 5;
        return sub(2);
    end function;

    function test24f(x : integer; r : real := 1.0) return integer;
    function test24f(y : integer; b : boolean := true) return integer;

    function test24 return integer is
    begin
        return test24f(x => 1) + test24f(y => 2);
    end function;

end package body;

package func2 is
    procedure test25(constant x : integer);
end package;

package body func2 is

    procedure test25(variable x : integer) is  -- Error
    begin
    end procedure;

    function test26(signal x : integer) return integer;

    function test26(x : integer) return integer is  -- Error
    begin
        return 1;
    end function;

end package body;

package func3 is
end package;

package body func3 is

   -- default class should be treated identically to constant class
   -- (ie, this should not produce an error)
    function issue182(bitv : bit_vector) return integer is
        function nested_fun return integer is
        begin
            return bitv'length;         -- OK
        end function;
    begin
        return nested_fun;
    end function;

    function issue123(signal x : integer) return integer is
        function nested return integer is
        begin
            return x + 1;               -- Error
        end function;
    begin
        return nested;
    end function;

    function constpure(x : integer) return integer is
        function nested return integer is
        begin
            return x + 1;               -- OK
        end function;
    begin
        return nested;
    end function;

    procedure notdef(x : integer) is
    begin
        fnork(x + 1);                   -- Error
    end procedure;

    procedure not_a_procedure is
    begin
        constpure(1);                   -- Error
        assert notdef(2) = 1;           -- Error
    end procedure;

    constant bad_c : bad_type := foo;       -- Error

    function bad_param ( constant x : bad_type := bad_c ) return bad_type;  -- Error

    function func1 (x : integer) return integer;

    function func1 (x : integer := 2) return integer is  -- Error
    begin
        return 1;
    end function;

    function func2 (x : integer := 1) return integer;

    function func2 (x : integer := 2) return integer is  -- Error
    begin
        return 1;
    end function;

    function func3 (x : bit := '1') return integer;

    function func3 (x : bit := '0') return integer is  -- Error
    begin
        return 1;
    end function;

    function func4 (x : integer) return integer;

    impure function func4 (x : integer) return integer is  -- Error
    begin
        return 1;
    end function;

    impure function func5 (x : integer) return integer;

    function func5 (x : integer) return integer is  -- Error
    begin
        return 1;
    end function;

    impure function func6 (variable x : integer) return integer;  -- Error

    impure function func7 (x : inout integer) return integer;  -- Error

end package body;
