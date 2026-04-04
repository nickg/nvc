package pack is
    constant c1 : integer := 42;

    function func return integer;
    function func (x : integer) return integer;
    function func (x, y, z : integer) return integer;
end package;

-------------------------------------------------------------------------------

entity test is
end entity;

use work.pack.all;

architecture test of test is
    component c1 is
        port (x : integer);
    end component;

    signal x : integer;
    signal r : real;
    signal v : bit_vector(1 to 8);

    type t_rec1 is record
        x, y : integer;
    end record;

    component c2 is
        port (x : t_rec1);
    end component;

    component c3 is
        port (x : bit_vector(1 to 3));
    end component;

    component c4 is
        port (x : out integer);
    end component;

    component c5 is
        generic (x : string);
    end component;
begin

    u1: component c1
        port map ( x => 1 );            -- OK

    u2: component c1
        port map ( x );                 -- OK

    u3: component c2
        port map ( x.x => x );          -- OK

    u4: component c1
        port map ( work.pack.c1 );      -- OK

    u5: component c3
        port map ( x(1) => '1',         -- OK
                   x(2 to 3) => "11" );  -- OK

    u6: component c4
        port map ( func(x) => x );      -- OK

    u7: component c4
        port map ( real(x) => r );      -- OK

    u8: component c1
        port map ( func(x, x, z => 1) );  -- OK

    u9: component c1
        port map ( work.pack.func(x => x) );  -- OK

    u10: component c3
        port map ( v(1 to 3) );         -- OK

    u11: component c5
        generic map ( x => "hello");    -- OK

    u12: component c5
        generic map ( x(1) => 'h', x(2) => 'e' );  -- OK

    u13: component c5
        generic map ( u12'instance_name );  -- OK

    u14: component c3
        port map ( bit_vector'(('0', '1')) );  -- OK

    u15: component c1
        port map ( work.pack.func );      -- OK


end architecture;
