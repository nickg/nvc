entity sub is
    generic (
        type t;                         -- OK
        INIT : t );                     -- OK
end entity;

architecture test of sub is
    constant myconst : t := INIT;       -- OK
    signal mysig : t;                   -- OK
    subtype mysub is t range 1 to 2;    -- Error
begin
end architecture;

-------------------------------------------------------------------------------

entity top is
end entity;

architecture test of top is
    component comp1 is
        generic (
            type t;                         -- OK
            INIT : t );                     -- OK
    end component;

    component comp2 is
    end component;

    component comp3 is
        generic (
            type t;                     -- OK
            function func1 (x : t) return t;  -- OK
            procedure proc1 (x : t) is <> );  -- OK
    end component;

    component comp4 is
        generic (
            type t;                     -- OK
            function func1 (x : t) return t is my_func;  -- OK
            procedure proc1 (x : t) is <> );  -- OK
    end component;

    for u3: comp2 use entity work.sub
        generic map ( t => real, init => 5.1 );  -- OK

    procedure proc1 ( a : integer );
    procedure proc1 ( b : real );

    function my_func (a : integer) return integer;
    function my_func (a : real) return real;

    function some_other_func (a, b : integer) return integer;

    type incomplete;
begin

    u1: entity work.sub
        generic map ( t => integer, init => 5 );  -- OK

    u2: component comp1
        generic map ( t => integer, init => 5 );  -- OK

    u3: component comp2;                -- OK

    u4: entity work.sub
        generic map ( t => integer, init => 1.2 );  -- Error

    u5: entity work.sub
        generic map ( real, "=", "/=", 5.2 );  -- Not sure: GHDL gives an error

    u6: entity work.sub
        generic map ( t => 5, init => 2 );  -- Error

    u7: component comp3
        generic map ( t => integer, func1 => my_func, proc1 => proc1 );  -- OK

    u8: component comp3
        generic map ( t => integer, func1 => my_func );  -- OK

    u9: component comp3
        generic map ( func1 => my_func );  -- Error

    u10: component comp3
        generic map ( t => bit, func1 => my_func );  -- Error

    u11: component comp3
        generic map ( t => real, func1 => my_func );  -- OK

    u12: component comp1
        generic map ( t => my_func, init => 5 );  -- Error

    u13: component comp1
        generic map ( t => u12, init => 5 );  -- Error

    u14: component comp1
        generic map ( t => incomplete, init => 1 );  -- Error

end architecture;
