package adder is
    generic (type t;
             function "+" (a, b : t) return t is <>;
             function to_string (x : t) return string is <>);
    function add (x, y : t) return t;
end package;

package body adder is
    function add (x, y : t) return t is
    begin
        return x + y;
    end function;
end package body;

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
        p : integer );
    port (
        x, y : in real;
        z : out real );
end entity;

architecture test of sub2 is
begin
end architecture;

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

    component sub2 is
        generic (
            package p is new work.adder generic map (<>) );
        port (
            x : in p.t;
            z : out p.t );
    end component;

    signal xr, zr : real;

    package real_adder is new work.adder generic map ( real );
begin

    u1: sub generic map ( real_adder ) port map ( xr, zr );
    u2: sub2 generic map ( real_adder ) port map ( xr, zr );

end architecture;
