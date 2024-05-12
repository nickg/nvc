package adder is
    generic (type t;
             function "+" (a, b : t) return t is <>;
             function to_string (x : t) return string is <>);
    function add (x, y : t) return t;
end package;

package body adder is
    function add (x, y : t) return t is
    begin
        report "add " & to_string(x) & " + " & to_string(y);
        return x + y;
    end function;
end package body;

-------------------------------------------------------------------------------

package components is
    component sub_c is
        generic (
            package p is new work.adder generic map (<>) );
        port (
            x : in p.t;
            z : out p.t );
    end component;
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

entity top is
end entity;

use work.components.all;

architecture test of top is
    signal xr, zr : real := 0.0;
    signal xi, zi : integer := 0;

    package real_adder is new work.adder generic map ( real );
    package int_adder is new work.adder generic map ( integer );
begin

    u1: sub_c generic map ( real_adder ) port map ( xr, zr );

    u2: sub_c generic map ( int_adder ) port map ( xi, zi );

    check: process is
    begin
        xr <= 1.0;
        wait for 1 ns;
        assert zr = 2.0;
        xr <= 5.0;
        wait for 1 ns;
        assert zr = 10.0;

        xi <= 8;
        wait for 1 ns;
        assert zi = 16;
        xi <= 100;
        wait for 1 ns;
        assert zi = 200;

        wait;
    end process;
end architecture;

-------------------------------------------------------------------------------

configuration genpack18 of top is
    for test
        for all : sub_c use entity work.sub
            port map ( x => x, y => x, z => z );
        end for;
    end for;
end configuration;
