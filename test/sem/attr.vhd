entity e is
end entity;

architecture a1 of e is
    attribute foo : integer;
    attribute bar : string;

    signal x, y, z : integer;

    attribute foo of x : signal is 6;   -- OK
    attribute bar of y : signal is "hello";  -- OK

    type int_vec is array (integer range <>) of integer;
    type int_vec_ptr is access int_vec;

    signal i : int_vec(1 to 3);

    attribute foo of i : signal is 6;  -- OK
begin

    process is
        variable v : integer;
    begin
        v := x'foo;                     -- OK
        report y'bar;                   -- OK
    end process;

    process is
    begin
        report z'foo;                   -- Error
    end process;

    process is
        variable v : int_vec_ptr;
    begin
        assert v'length = 5;            -- OK
        assert v.all'length = 62;       -- OK
    end process;

    process is
    begin
        report e'path_name;             -- OK
        report e'instance_name;         -- OK
        report a1'path_name;            -- OK
        report a1'instance_name;        -- OK
    end process;

    process is
    begin
        assert i'event;                 -- OK
        assert i(1)'event;              -- OK
        assert i(x)'event;              -- OK
        assert i'foo = 1;               -- OK
        assert i(1)'foo = 2;            -- Error
    end process;

end architecture;

architecture a2 of e is
    attribute foo : integer;
    attribute bar : string;

    signal x, y, z : integer;

    attribute foo of z : signal is string'("boo");  -- Error
    attribute bar of x : signal is 73;  -- Error
    attribute foo of q : signal is 71;  -- Error
    attribute foo of yah : label is 12;  -- Ignored

begin
end architecture;

architecture a3 of e is
    type int10_vec is array (integer range 1 to 10) of integer;
begin

    process is
        variable x : integer;
    begin
        assert int10_vec'low = 1;       -- OK
        assert int10_vec'high = 10;     -- OK
        assert int10_vec'left = 1;      -- OK
        assert int10_vec'right = 10;    -- OK
        assert int10_vec'low(1) = 1;    -- OK
        assert int10_vec'left(x) = 2;   -- Error
    end process;

end architecture;

package pack is
    function func(x : in integer) return integer;
    attribute p : POSITIVE;
    attribute p of pack : package is 10; -- OK
end package;

package body pack is
    function func(x : in integer) return integer is
    begin
        report func'instance_name;      -- OK
        report x'simple_name;           -- OK
        report true'simple_name;        -- Error
        report pack'path_name;          -- OK
        report integer'path_name;       -- Error
        return x + 1;
    end function;
end package body;

entity issue39 is
    generic (
        g : bit := '0'
    );
begin
    assert (g = '0' or g = '1')
        report issue39'instance_name & "oops!"
        severity failure;
end entity issue39;

architecture a4 of e is
begin
    process is
    begin
        assert integer'image(0)(0) = '0';  -- OK
    end process;

    process is
        variable i : integer;
        attribute a : bit_vector;
        attribute a of i : variable is "101";
        attribute b : integer;
        attribute b of i : variable is 4;
    begin
        assert i'a(1) = '0';            -- OK
        assert i'b(1) = 1;              -- Error
    end process;

    process is
        variable i : integer;
        attribute a : boolean;
        attribute a of i : signal is true;  -- Error
    begin
    end process;

    process is
        variable x : integer;
    begin
        assert x'last_event = 0 ns;     -- Error
    end process;

    process is
        type bv_ptr is access bit_vector;
        variable a : bv_ptr;
        type r is record
            x : integer;
        end record;
        variable b : r;
    begin
        a(a'range) := "110101";           -- OK
        a(bit_vector'range) := "110101";  -- Error
        a(b'range) := "101010";           -- Error
        a(e'range) := "110101";           -- Error
    end process;

    process is
        function func(x : integer) return bit_vector;
        variable a : bit_vector(1 to 10);
    begin
        a(func(4)'range) := (others => '1');  -- OK
    end process;

    process is
        type bvptr is access bit_vector;
        variable b : bvptr;
    begin
        for i in b.all'range loop       -- OK
        end loop;
        for i in b'range loop           -- OK
        end loop;
    end process;

    b1: block is
        function fie  return string is
        begin
            return "11010011";
        end function;

        function fie2(x : integer := 4) return string is
        begin
            -- report fie2'instance_name;   ???
            return "101";
        end function;
    begin
        process
        begin
            assert fie'RIGHT = 1;  -- OK
            assert fie2'RIGHT = 1;  -- OK
        end process;
    end block;

    process
        type int2_vec is array (66 to 67) of integer;
        variable b : boolean;
        variable x : int2_vec;
    begin
        b := a4'length = 5;             -- Error
        b := x'length = 5;              -- OK
        b := x'low(1) = 1;              -- OK
        b := x'high(1) = 5;             -- OK
        b := x'left = 1;                -- OK
        b := x'right = 5;               -- OK
        b := int2_vec'length = 2;       -- OK
        b := int2_vec'low = 66;         -- OK
        report int2_vec'image(x);       -- Error
    end process;

    process is
        subtype my_int is integer range 1 to 20;
        subtype my_bool is boolean range true to true;
        variable x : integer;
        alias my_int_base is my_int'base;  -- OK
    begin
        assert my_int'base'left = 1;    -- OK
        assert x'base'left = 2;         -- Error
        assert my_int'base = 5;         -- Error
        assert my_bool'base'pos(true) = 1;  -- OK
        report my_bool'base'image(true);  -- OK
    end process;

    process is
        variable x : not_here;          -- Errror
    begin
        assert x'length = 0;            -- Error (suppressed)
    end process;

    process is
        -- Test for VHDL-2019 attributes used in -93
        attribute index : integer;
        attribute designated_type : integer;
        attribute converse : integer;
        variable a : integer;
        attribute index of a : variable is 4;  -- OK
        attribute designated_type of a : variable is 4;  -- OK
        attribute converse of a : variable is 4;  -- OK
    begin
        assert a'index = 4;             -- OK
        assert a'designated_type = 4;   -- OK
        assert a'converse = 4;          -- OK
    end process;

    process is
    begin
        assert integer'val(1) = 1;      -- OK
        assert integer'val(1.2) = 1;    -- Error
        assert integer'val;             -- Error
        assert integer'pred(15) = 14;   -- OK
        assert integer'succ(15) = 16;   -- OK
    end process;

    process is
        type bit2d is array (1 to 2, 1 to 2) of bit;
    begin
        assert bit2d'value("1010") = (('1','0'),('1','0'));  -- Error
    end process;

end architecture;

use work.pack.all;
entity tc3109 is
  attribute p of tc3109 : entity is 20;  -- OK
end tc3109;

architecture tc3109arch of tc3109 is
  attribute p    of tc3109arch : architecture is 30;  -- OK
begin
  testing: process
  begin
    assert not(   tc3109'p   = 20   and  -- OK
                  tc3109arch'p = 30   ); -- OK
    assert (   tc3109'p   = 20   and    -- OK
               tc3109arch'p = 30   );   -- OK
    wait;
  end process testing;

end;

ARCHITECTURE c07s04b01x00p08n01i02565arch OF e IS
  SUBTYPE s10 IS STRING (1 TO 4);
  ATTRIBUTE attr1 : INTEGER;
  ATTRIBUTE attr1 OF s10 : SUBTYPE IS 4;  -- OK
begin
  TESTING: PROCESS
    VARIABLE v : s10;
  BEGIN
    v := (1 | s10'attr1 => 'a', OTHERS => 'b' );  -- OK
    wait for 5 ns;
    wait;
  END PROCESS TESTING;
END c07s04b01x00p08n01i02565arch;

architecture builtins of e is
    procedure p;
    attribute never_waits : boolean;
    attribute never_waits of p : procedure is true;  -- OK
    attribute never_waits of p : procedure is false;  -- OK
    attribute never_waits of p : procedure is 1 = 1;  -- Error
    function f return integer;
    attribute never_waits of f : function is true;   -- Error
    procedure q (x : integer);
    procedure q (x : boolean);
    attribute never_waits of q [integer] : procedure is true;  -- OK
    attribute foreign of f [return integer] : function is "bad string";  -- OK
    attribute foreign of f [return integer] : function is e'path_name;  -- OK
begin

end architecture;
