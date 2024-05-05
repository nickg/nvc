package adder is
    generic (type t;
             function "+" (a, b : t) return t is <>);
    function add (x, y : t) return t;
end package;

package body adder is
    function add (x, y : t) return t is
    begin
        return x + y;
    end function;
end package body;

-------------------------------------------------------------------------------

package adder2 is
    generic (type t;
             function "+" (a, b : t) return t is <>);
end package;

-------------------------------------------------------------------------------

entity sub is
    generic (
        package p is new work.adder generic map (<>) );
    port (
        x, y : in p.t;
        z : out p.t );
end entity;

architecture test of sub is
begin
    z <= p.add(x, y);
end architecture;

-------------------------------------------------------------------------------

entity sub2 is
    generic (
        package p is new work.adder2 generic map (<>) );
    port (
        x, y : in p.t;
        z : out p.t );
end entity;

-------------------------------------------------------------------------------

entity top is
end entity;

architecture test of top is
    component sub is
        generic (
            package p is new work.adder generic map (<>) );
        port (
            x : in p.t;
            z : out p.t );
    end component;

    signal xr, zr : real;
    signal xi, zi : integer;

    package real_adder is new work.adder generic map ( real );
    package int_adder is new work.adder generic map ( integer );
begin

    u1: sub generic map ( real_adder ) port map ( xr, zr );  -- OK

    u2: sub generic map ( real_adder ) port map ( xi, open );  -- Error

    u3: sub generic map ( real_adder ) port map ( xr, zr );  -- OK

end architecture;

-------------------------------------------------------------------------------

configuration genpack17 of top is
    for test
        for u1 : sub use entity work.sub
            port map ( y => x );
        end for;
        for u3 : sub use entity work.sub2
            port map ( y => x );        -- Error
        end for;
    end for;
end configuration;
