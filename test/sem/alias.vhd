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

begin
end architecture;
