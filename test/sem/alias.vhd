entity e is
end entity;

architecture test of e is

    alias my_int is integer;            -- OK
    signal x : my_int;                  -- OK
    subtype s is my_int range 1 to 5;   -- OK

    alias my_bad : integer is integer;  -- Error
    alias ax is x;                      -- OK
    signal y : ax;                      -- Error

    alias as is s;                      -- OK
    signal z : as;                      -- OK

    function foo (x : bit) return integer;
    function foo (x : character) return integer;

    alias foo_bit is foo [bit return integer];  -- OK
    alias foo_char is foo [character return integer];  -- OK
    alias foo_int is foo [integer return integer];  -- Error
    alias foo_p is foo [bit];           -- Error
    alias foo_a is foo(1) [bit return integer];  -- Error
    alias foo_b is foo [blah return integer];  -- Error

    procedure bar (x : bit);
    procedure bar (x : character);

    alias bar_bit is bar [bit];         -- OK
    alias bar_char is bar [character];  -- OK
    alias bar_int is bar [integer];     -- Error

    procedure test is
    begin
        assert foo_bit('1') = 1;        -- OK
        assert foo_char('1') = 1;       -- OK
        bar_bit('1');                   -- OK
        bar_char('1');                  -- OK
        assert foo('1') = 1;            -- Error
        assert foo_int(1) = 1;          -- Error
        bar_bit(character'( '1' ));     -- Error
        bar_bit(character'(1));         -- Error
    end procedure;

    type bv_ptr is access bit_vector;

    procedure test2(variable x : bv_ptr) is
        variable v : bit_vector(1 to 10);
        alias va is v(x'left);  -- Error
    begin
    end procedure;

    procedure maybe_use_last_value(signal x : my_int);

    procedure proc is
    begin
        maybe_use_last_value(ax);
    end procedure;

    type int_array is array (integer range <>) of integer;
    alias int_vector is int_array;
    type int_array_2 is array (integer range <>) of integer;

    constant c1 : int_array_2(1 to 3) := (1, 2, 3);
    constant c2 : int_vector(1 to 3) := int_vector(c1);  -- OK

    subtype bad is blah blah blah;  -- Error

    function foo_bad (x : bit) return bad;  -- OK
    function foo_bad (x : bit) return bad;  -- Error (suppressed)

    alias foo_o is foo_bad [bit return bad];  -- OK
    alias foo_o is foo_bad [character return bad];  -- Error (suppressed)

    alias my_now is std.standard.now [return delay_length];  -- OK
    alias my_eq is std.standard."=" [bit, bit return boolean];  -- OK

    type int_mat2d is array (integer range <>, integer range <>) of integer;
    constant c3 : int_mat2d(1 to 2, 1 to 2) := ((1, 2), (3, 4));
    alias c2_alias is c3;               -- Error (in '93)

    constant c4 : integer := int_vector;  -- Error

    type line is access string;
    procedure access_alias ( variable p : inout line ) is
        variable l : line;
        alias a : string(1 to 3) is l.all;  -- Error
        alias b : string(1 to p'length) is p.all;  -- Error
    begin
    end procedure;

    constant LEN : natural := 20 ;
    function doit return bit_vector is
        alias length : natural is LEN ;
        -- Following is OK, from UVVM
        constant rv : bit_vector(length-1 downto 0) := (length-1 => '1', others =>'0') ;
    begin
        return rv ;
    end function ;

    procedure double_alias is
        type r is record
            x, y : integer;
        end record;
        alias a1 is r;
        alias a2 is a1;                 -- OK
        variable v : a2;                -- OK
    begin
    end procedure;

    function less (x, y : int_vector) return boolean;
    alias "<=" is less[int_vector, int_vector return boolean];  -- OK
    constant c5 : boolean := int_vector'(1, 2) <= (1, 3);  -- OK

begin
end architecture;
