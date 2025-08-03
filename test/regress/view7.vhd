-- Test that 'converse attribute is applied recursively to nested views
package test_pack is
    type inner_rec_t is record
        a : natural;
        b : natural;
    end record;

    type outer_rec_t is record
        x : natural;
        inner : inner_rec_t;
    end record;

    view inner_view of inner_rec_t is
        a : in;
        b : out;
    end view;

    view outer_view of outer_rec_t is
        x : in;
        inner : view inner_view;
    end view;

    alias converse_outer_view is outer_view'converse;
    -- In converse_outer_view:
    -- inner.a should be OUT
    -- inner.b should be IN
end package;

use work.test_pack.all;

-- Test entity that validates the converse view modes
entity view7 is
    port (
        p : view converse_outer_view
    );
end entity;

architecture test of view7 is
begin
    -- This assignment only compiles if converse was applied to nested view
    p.inner.a <= 42;

    p.x <= 10;
    p.inner.b <= p.inner.b + 1;

    std.env.finish;
end architecture;
