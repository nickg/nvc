-- Test that 'converse attribute is applied recursively to nested views
library ieee;
use ieee.std_logic_1164.all;
package test_pack_generic is
    generic (
        n: integer;
    );

    type inner_rec_t is record
        a : natural;
        b : natural;
    end record;

    type outer_rec_t is record
        x : natural;
        y: std_logic_vector(n-1 downto 0);
        inner : inner_rec_t;
    end record;

    view inner_view of inner_rec_t is
        a : in;
        b : out;
    end view;
    alias converse_inner_view is inner_view'converse;

    view outer_view of outer_rec_t is
        x, y : in;
        inner : view inner_view;
    end view;
    alias converse_outer_view is outer_view'converse;
end package;

package test_pack is new work.test_pack_generic generic map (n => 8);

entity foo is
    port (
        x: in natural;
        a: out natural;
        b: in natural;
    );
end entity;
architecture test of foo is
begin
    a <= b + 1;
end architecture;

use work.test_pack.all;
use work.foo;

entity baz is
    port (
        p : view converse_outer_view
    );
end entity;
architecture test of baz is
begin
    inst2: entity foo port map(
        x => p.x,
        a => p.inner.a, -- p.inner.a should resolve to an out
        b => p.inner.b
    );
end architecture;

use work.test_pack.all;
use work.baz;

entity view7 is
end entity;
architecture test of view7 is
    signal x: natural := 1;
    signal inner: inner_rec_t := ( a => 2, b => 3 );
begin
    inst: entity baz port map(
        p.x => x,
        p.inner => inner
    );
    check: process is
    begin
        inner.b <= 5;
        wait for 1 ns;
        assert inner.a = 6;
        std.env.finish;
    end process;
end architecture;
