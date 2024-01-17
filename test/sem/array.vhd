package p is

    type int_array is array (integer range <>) of integer;

    type ten_ints is array (1 to 10) of integer;

end package;

entity ent is
end entity;

use work.p.all;

architecture arch of ent is
    -- All these declarations are OK
    signal x : int_array(1 to 5);
    signal y : ten_ints;
    signal z : int_array(1 to 3) := ( 0, 1, 2 );
    signal m : int_array(1 to 3) := ( 1 to 3 => 0 );
    alias a : int_array(2 to 3) is x(2 to 3);

begin

    process is
        -- Positional elements cannot follow named
        variable e : int_array(1 to 2) := (
            0 => 1, 2 );
    begin
    end process;

    process is
        -- Others element must be last
        variable e : ten_ints := ( others => 5, 1 => 2 );
    begin
    end process;

    process is
        -- Only one others element
        variable e : ten_ints := ( others => 5, others => 2 );
    begin
    end process;

    process is
        -- Single element aggregates must be named
        variable a : int_array(0 to 0) := ( 0 => 1 );
        variable b : int_array(0 to 0) := ( 1 );  -- Error
    begin
    end process;

    process is
        variable a : integer;
    begin
        x(0) <= 1;                      -- OK
        x <= ( others => 2 );           -- OK
        x <= 1;                         -- RHS not array
        a := x(0);                      -- OK
        a := x;                         -- LHS not array
    end process;

    process is
        variable b : boolean;
    begin
        b := z = m;                     -- OK
        b := z /= m;                    -- OK
        b := z = y;                     -- Different types
    end process;

    process is
    begin
        x(1 to 3) <= z;
        x(1 to 2) <= z(1 to 2);
        x(x'range) <= (others => 0);
    end process;

    process is
    begin
        a(2) <= 4;                      -- OK
        y(2) <= 1;                      -- OK
    end process;

    process is
        type int2d is array (1 to 10, 1 to 4) of integer;
        variable w : int2d := ( 1 => ( 1, 2, 3, 4 ),
                                2 => ( others => 5 ),
                                others => ( others => 0 ) );
    begin
        w(2, 4) := 6;
        w(6) := 6;                      -- Too few indices
        w(6, 7, 2) := 2;                -- Too many indices
    end process;

    process is
        type letter is (A, B, C);
        type larray is array (letter) of integer;
        variable w : larray;
    begin
        w(A) := 2;                      -- OK
        w(5) := 66;                     -- Wrong index type
    end process;

    process is
        variable n : int_array(1 to 3) := ( 0, 1 => 1, others => 2 );  -- Error
    begin
    end process;

    process is
        variable x : integer;
        constant c : integer := 3;
        variable y : int_array(1 to 3);
    begin
        y := ( 1 => 2, 2 => 3, x => 5 );  -- Error
        y := ( 1 => 2, 2 => 3, c => 5 );  -- OK
    end process;

    process is
        variable x : integer;
        variable y : int_array(3 downto 0);
    begin
        x(1 to 3) := (others => 4);     -- Error
        y(1 to 3) := (others => 4);     -- Error
        assert y = (others => 4);       -- Error
    end process;

    process is
        subtype four_ints is int_array(1 to 4);
        variable x : four_ints;
    begin
        x(1 to 3) := (1, 2, 3);         -- OK
        x(2) := 1;                      -- OK
        x(3 downto 1) := (others => '0');  -- Error
        assert x(2) = 5;                -- OK
    end process;

    process is
        function foo(size: integer) return int_array is
            subtype rtype is int_array(size-1 downto 0);
            variable result: rtype;
        begin
            assert result(0) = 1;
            return result;
        end;
    begin
    end process;

    process is
        function plus(A, B: int_array) return int_array is
            variable BV, sum: int_array(A'left downto 0);
        begin
            return sum;
        end;
    begin
    end process;

    process is
        subtype int4_t is int_array(1 to 4);
        type foo_t is array (integer'left to 10) of integer;
        variable v : int_array(foo_t'range);
        variable u : foo_t;
    begin
        assert int4_t'length = 4;
        assert foo_t'length = 50;
    end process;

    process is
        subtype a_to_c is character range 'a' to 'c';
        type abc_ints is array (a_to_c) of integer;
        variable v : abc_ints;
    begin
        assert abc_ints'length = 3;
        v('b') := 2;
    end process;

    process is
        type bit_map is array (bit) of integer;
        variable b : bit_map := ( '0' => 5, '1' => 6 );
        type bit_map2 is array (bit, 0 to 1) of integer;
        variable c : bit_map2 := (
            '0' => (0 => 0, 1 => 1),
            '1' => (0 => 2, 1 => 3) );
    begin
        b('0') := 6;
        c('1', 1) := 5;
    end process;

    process is
        constant c : ten_ints := (ten_ints'range => 5);
        variable v : ten_ints;
    begin
        v := (v'range => 6);            -- OK
    end process;

    process is
        type mybit is ('0', '1');
        type bit_map is array (bit range '0' to '1') of integer;
        variable v : bit_map;
        variable b : bit;
    begin
        v(b) := 1;                      -- OK
    end process;

    process is
    begin
        assert x'length(1) = 5;         -- OK
    end process;

    process is
        type bad is array (integer range <>) of int_array;  -- Error
    begin
    end process;

    process is
        type int2d is array (natural range <>, natural range <>) of integer;
        constant c : int2d := ( (0, 1, 2), (0, 1, 2) );  -- OK
        constant d : int2d := ( (0, 1), (5, 6, 7) );  -- OK (at sem)
        constant e : int2d := ( (0, 1), (others => 2) );  -- Error
    begin
    end process;

    process is
        variable b1 : bit_vector(7 downto 0);
    begin
        b1 := b1 sll 1;
        b1 := b1 srl 2;
        b1 := b1 sla 0;
        b1 := b1 sra 1;
        b1 := b1 rol 6;
        b1 := b1 ror 1;
    end process;

    process is
        variable i : integer;
        alias xi is x(1 to i);          -- Error
        alias zi : integer is z(i);     -- Error
        alias xx : integer is x(1 to 2);  -- Error
    begin
    end process;

    process is
        variable i : integer;
    begin
        i(6) := 2;                      -- Error
    end process;

    process is
        constant c : integer := -1;
        type bad_range is array (-1 to -5) of integer;  -- OK
        type ok_range is array(c to -5) of integer;  -- OK
    begin
    end process;

    process is
        subtype bad_sub1 is int_array(1 to 3, 2 to 5);  -- Error
    begin
    end process;

    process is
        type element is array (integer range 0 to 1) of bit_vector( 0 to 1);
    begin
    end process;

    process is
        type ten_ten_ints is array (1 to 10) of ten_ints;
        type int2d is array (natural range <>, natural range <>) of integer;
        variable t1, t2 : ten_ten_ints;
        variable m1, m2 : int2d(1 to 3, 1 to 3);
    begin
        assert t1 = t2;                 -- OK
        assert t1 /= t2;                -- OK
        assert t1 < t2;                 -- Error
        assert t1 > t2;                 -- Error
        assert m1 = m2;                 -- OK
        assert m1 < m2;                 -- Error
    end process;

    process is
        subtype num_array is int_array;   -- OK
        subtype bad_array is not_here;    -- Error
        variable a1 : num_array(1 to 3);  -- OK
        variable a2 : num_array;          -- Error
    begin
    end process;

    process is
        constant k : integer := 5;
        type a is array (k) of integer;  -- Error
        variable v : a;                 -- Error
    begin
    end process;

    process is
        type ibv is array (boolean range <>) of integer;
        variable a : ibv(false to true);
    begin
        a(false) := 1;                  -- OK
        a(4) := 2;                      -- Error
        a(false to false) := (others => 1);  -- OK
    end process;

    process is
        subtype r is integer range 1 to 3;
    begin
        x(r'range) <= (others => 1);
        x(r) <= (others => 1);
    end process;

    process is
        subtype str is string;
        constant x : str := "hello";  -- OK
    begin
    end process;

    process is
        type barry2d is array (boolean range <>, boolean range <>)
            of integer;
        variable b : barry2d(false to true, false to true);
        type ibarray2d is array (integer range <>, boolean range <>)
            of integer;
        variable ib : ibarray2d(1 to 5, false to true);
    begin
        b(barry2d'left(1), barry2d'left(2)) := 5;  -- OK
        ib(integer'(5), boolean'(true)) := 1;      -- OK
        ib(ibarray2d'left(1), ibarray2d'left(2)) := 5;  -- OK
    end process;

    process is
        type enum1 is (m1, m2, m3, m4, m5);
        type abase is array (enum1 range <>) of boolean;
        subtype a1 is abase(enum1 range m1 to m5);
        variable V1 : A1;
    begin
        assert v1 = (false, false, false);  -- OK
    end process;

    process is
        variable x : int_array(1 to 3);
    begin
        x := (1 | 2 to 3 => 5);         -- OK
    end process;

    process is
        variable b : bit_vector(1 to 3);  -- OK
    begin
        b := "1fe";                     -- Error
    end process;

    issue86: block is
        type integer_vector is array (natural range <>) of integer;
        subtype ElementType        is integer ;
        subtype ArrayofElementType is integer_vector;

        function inside0 (constant E : ElementType;
                          constant A : in ArrayofElementType) return boolean is
        begin
            for i in A'range loop       -- OK (issue #86)
                if E = A(i) then
                    return TRUE;
                end if ;
            end loop ;
            return FALSE ;
        end function inside0;
    begin
    end block;

    process is
        subtype bad is ten_ints (1 to 4);               -- Error
        constant c : ten_ints(2 to 4) := (others => 0); -- Error
    begin
    end process;

    process is
        type e is (one, two, three);
        type arr is array (e range <>) of integer;
        constant c : arr := (1, 2, 3, 4);
    begin
    end process;

    no_file_types: block is
        type t_int_file   is file of integer;
        type t_file_array is array (0 to 1) of t_int_file;  -- Error
    begin
    end block;

    billowitch_tc586: block is
        type real_cons_vector  is array (15 downto 0) of real;
        type real_cons_vector_file is file of real_cons_vector;
        constant C19 : real_cons_vector := (others => 3.0);  -- OK
    begin
    end block;

    process is
        type bad1 is array (real range <>) of real;  -- Error
        type bad2 is array (natural range <>, bit_vector range <>) of bit;  -- Error
    begin
    end process;

    process is
        variable n : positive;
    begin
        x <= (true => 1, others => 0);  -- Error
        x <= (1 to true => 0);          -- Error
        x <= (false to true => 0, 1 => 1);  -- Error
        x <= (n to n + 4 => n);         -- OK
        x <= (n => 3, others => 0);     -- Error
        x <= (1 => 0, 2 to n => 1);     -- Error
        x <= (1 to n => 1);             -- OK
    end process;

    process is
        procedure p(l : natural) is
            variable v : int_array(1 to l);
        begin
            v := (1 => 0, others => 1);  -- Error
        end procedure;
    begin
    end process;

    process is
        function f(b:integer:=0) return string is begin return "abc"; end function;
        function f               return string is begin return "def"; end function;
        alias f0 is f [integer return string];  -- OK
        subtype r is integer range 1 to 2;
    begin
        report "x: " & f0(r) severity note;  -- OK
        report "x: " & f(r) severity note;   -- Error
    end process;

    process is
        type bit_map is array (bit) of integer;
        function f return bit_map is begin return (0, 1); end function;
    begin
        assert f(bit) = (0, 1);                 -- OK
        assert f(std.standard.bit) = (0, 1);    -- OK
    end process;

    process is
        type a1 is array (0.0 to 7) of real;  -- Error
    begin
    end process;

    process is
    begin
        assert m'range(5)'left = 2;     -- Error
    end process;

    process is
        variable v : foo;               -- Error
    begin
        v(0) := 1;                      -- Error
        v(1 to 3) := 0;                 -- Error
    end process;

    process is
        type t_alert_level is (NO_ALERT, NOTE, TB_NOTE, WARNING);
        type t_alert_counters is array (NOTE to t_alert_level'right) of natural;
    begin
    end process;

    process is
        -- Reduced from UVVM case
        type rec is record
            x : string(1 to 3);
        end record;
        procedure proc (variable r : out rec) is
            alias a : string(r.x'range) is r.x;
        begin
            a(1 to 3) := "abc";         -- OK
        end procedure;
    begin
    end process;

    issue685: process is
        subtype byte is bit_vector(7 downto 0);
        type byte_vector is array (natural range <>) of byte;
        type my_record is record
            a : byte_vector(0 to 4);
        end record my_record;
        constant MY_CONST : my_record := (a => (others => x"00"));
        function to_hstring(x : bit_vector) return string;
    begin
        report to_hstring(MY_CONST(0));  -- Error
    end process;

    issue835: process is
        type t_my_bv is array (natural range <>) of bit;
        variable x : bit_vector(1 to 7);
        variable y : t_my_bv(1 to 4);
    begin
        y(1) := t_my_bv(x)(1);          -- Error
        y := t_my_bv(x)(1 to 4);        -- Error
    end process;

    process is
        type t_int2d is array (natural range <>, natural range <>) of integer;
        type t_int2d_ptr is access t_int2d;
        variable p : t_int2d_ptr;
    begin
        p.all(1) := 2;             -- Error
    end process;

end architecture;
