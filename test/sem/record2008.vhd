entity record2008 is
end entity;

architecture test of record2008 is
    type rec1 is record
        x : bit_vector;                 -- OK
    end record;

    constant c1 : rec1 := ( x => "101" );  -- OK
    signal s1 : rec1;                   -- Error
    signal r1 : rec1(x(1 to 3));        -- OK
    signal r2 : rec1(y(1 to 3));        -- Error
    signal r3 : rec1(r2(1 to 2));       -- Error

    type rec2 is record
        x, y : bit_vector;              -- OK
        a : bit_vector(1 to 3);
        b : integer;
    end record;

    signal r4 : rec2(x(1 to 3), y(1 to 4));  -- OK
    signal r5 : rec2(x(1 to 3), x(1 to 4));  -- Error
    signal r6 : rec2(x(1 to 3), p(1 to 4));  -- Error
    signal r7 : rec2(b(1 to 3));        -- Error
    signal r8 : rec2(x(1 to 3));        -- Error

    subtype t1 is rec1(x(1 to 3));      -- OK
    signal r9 : t1;                     -- OK
    subtype t2 is rec1;                 -- OK
    signal r10 : t2;                    -- Error
    subtype t3 is rec2(x(1 to 5));      -- OK
    signal r11 : t3(y(1 to 2));         -- OK
    subtype t4 is t3(y(1 to 1));        -- OK
    subtype t5 is t4(x(1 to 2));        -- Error

    type rec3 is record
        r : rec1;                       -- OK
        s : bit_vector;                 -- OK
    end record;

    signal r12 : rec3;                  -- Error
    signal r13 : rec3(r(x(1 to 3)), s(1 to 2));  -- OK
    signal r14 : rec3(s(1 to 2));       -- Error
begin

    p1: process is
    begin
        r4.x <= (others => '0');        -- OK
    end process;

    -- From UVVM
    p2: process is
        type unsigned is array(natural range <>) of bit ;
        type some_config is record
            a   :   unsigned ;
            b   :   integer ;
        end record ;
        constant FINAL_CONFIG : some_config(a(0 downto 0)) := (
            a   =>  (others =>'0'),     -- OK
            b   =>  20 ) ;
        constant c2 :  some_config(a(0 downto 0)) := (
            (others =>'0'), 20 ) ;      -- OK
    begin
    end process;

    p3: process is
        type rec1_vec is array (natural range <>) of rec1;
        constant c1 : rec1_vec(0 to 0)(x(1 to 3)) := (  -- OK
            0 => (x => "111") );
    begin
    end process;

    -- From "abc" test case provided by Brian Padalino
    p4: process is
        type Parameters_t is record
            BW    : natural;
            PAIRS : natural;
        end record;

        type Indices_t is array (natural range <>) of bit_vector;

        type Bus_t is record
            Indices : Indices_t;
        end record;

        function Test(
            abc_bus : Bus_t;
            indices : Indices_t
            ) return Bus_t is
            variable result : Bus_t(
                Indices(abc_bus.Indices'range)(abc_bus.Indices'element'range)
                );                      -- OK
        begin
            return result;
        end function;
    begin
    end process;

    p5: process is
        procedure test (r : rec1) is
            variable y : r'subtype;     -- OK
            alias yx : y.x'subtype is y.x;  -- OK
        begin
        end procedure;
    begin
    end process;

    p6: process is
        type rec1_array is array (natural range <>) of rec1;
        variable a1 : rec1_array(1 to 3);  -- Error
    begin
    end process;

end architecture;
