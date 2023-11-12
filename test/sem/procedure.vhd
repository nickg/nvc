package p is

    procedure foo(x : in integer; y : out integer);

    procedure yah is                    -- Error
    begin
        null;
    end procedure;

end package;

package body p is

    procedure foo(x : in integer; y : out integer) is
        variable i : integer;
    begin
        y := x + 1;
    end procedure;

    procedure bar(x : in integer; signal y : out integer) is
    begin
        y <= x + 1;
    end procedure;

    procedure yam is
    begin
        return;                         -- OK
        return 5;                       -- Error
    end procedure;

    procedure foo_wrap(y : out integer) is
    begin
        foo(5, y);
    end procedure;

    procedure has_def(x : in integer; y : in integer := 7) is
    begin
    end procedure;

    procedure calls_has_def is
    begin
        has_def(5);
    end procedure;

    procedure bad_def(x : in bit := 6) is
    begin
    end procedure;

    procedure bad_def2(x : in bit := '1'; y : in integer) is
    begin
    end procedure;

    procedure diff_types(x : in integer; y : in string) is
    begin
    end procedure;

    procedure test_named is
    begin
        diff_types(1, "foo");            -- OK
        diff_types(1, y => "bar");       -- OK
        diff_types(x => 1, y => "foo");  -- OK
        diff_types(y => "la", x => 6);   -- OK
        diff_types(y => "foo");          -- Error
        diff_types(y => "f", 6);         -- Error
    end procedure;

    procedure overload(x : in bit) is
    begin
    end procedure;

    procedure overload(x : in integer) is
    begin
    end procedure;

    procedure test_overload is
    begin
        overload('1');
        overload(1);
    end procedure;

    procedure test1(x : in integer; y : out integer) is
    begin
        y := y + 1;                     -- Error
        x := 6;
    end procedure;

    procedure test2(signal x : in bit) is
    begin
        -- These are errors according to LRM 93 section 2.1.1.2
        assert x'stable;
        assert x'quiet;
        assert x'transaction = '1';
        assert x'delayed(1 ns) = '1';
    end procedure;

    type int_ptr is access integer;

    procedure test3(constant x : inout int_ptr);  -- Error
    procedure test4(x : in int_ptr);  -- Error
    procedure test4a(x : int_ptr);  -- Error
    procedure test4b(x : out int_ptr);  -- OK

    procedure test5_a(variable x : integer) is
    begin
    end procedure;

    procedure test5_b(variable x : integer) is
        alias a : integer is x;
    begin
        test5_a(a);
    end procedure;

    type int2d is array (natural range <>, natural range <>) of integer;

    procedure test6 (
        variable a : inout bit_vector;
        constant b : in int2d;
        constant c : in natural ) is
    begin
    end procedure;

    procedure test6 (
        variable a : inout bit_vector;
        constant b : in int2d ) is
    begin
        test6 ( b => b,
                c => 1,
                a => a );
    end procedure;

    procedure test7a(x : in bit_vector(1 to 2)) is
    begin
    end procedure;

    procedure test7b is
    begin
        test7a(x(1) => '0', x(2) => '1');  -- OK
    end procedure;

    procedure test8(x : out int_ptr) is
    begin
        if x /= null then               -- Error
        end if;
    end procedure;

    procedure test9(x : out integer) is
    begin
        x <= 5;                         -- Error
    end procedure;

    type t_access_array is array (0 to 1) of int_ptr;
    type t_access_record is record
        a   : integer;
        b   : int_ptr;
    end record;

    procedure test10(constant arg : t_access_array) is   -- Error
    begin
        null;
    end procedure;

    procedure test11(constant arg : t_access_record) is  -- Error
    begin
        null;
    end procedure;

    procedure test12(signal   arg : t_access_array) is   -- Error
    begin
        null;
    end procedure;

    procedure test13(signal   arg : t_access_record) is  -- Error
    begin
        null;
    end procedure;

    procedure test14 is
        variable x : bit_vector(1 to 3);
    begin
        x(1);                           -- Error
    end procedure;

    procedure test15 (signal x : out bit bus) is  -- Error
    begin
    end procedure;

    procedure test16 (signal x : in bit_vector(1 to 3)) is
        procedure test16_a (signal y : bit) is
        begin
        end procedure;
        variable i : integer;
    begin
        test16_a(x(i));                 -- Error, not static name
    end procedure;

    procedure test17 is
        procedure test17_a (x, y : integer) is
        begin
        end procedure;
    begin
        test17_a(x => 1, x => 2);       -- Error
        test17_a(z => 1, x => 2);       -- Error
    end procedure;

    procedure test18 (signal x : in bit) is
    begin
        x <= '1';                       -- Error
    end procedure;

end package body;
