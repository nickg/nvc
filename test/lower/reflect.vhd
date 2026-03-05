use std.reflection.all ;

entity test is
end entity ;

architecture arch of test is

    type myint_t is range -5 to 5 ;

    type a_t is array(myint_t range -1 to 1) of bit ;
    type b_t is array(myint_t) of bit ;
    type c_t is array(-1 to 1) of bit ;

begin

    tb : process
        variable stm  : subtype_mirror ;
        variable astm : array_subtype_mirror ;
    begin

        -- a_t: constrained 1-D array with a custom integer index type.
        -- Bounds should reflect the constrained range, not the full myint_t range.
        stm := a_t'reflect ;
        assert stm.get_type_class = CLASS_ARRAY
            report "FAIL: a_t class" severity failure ;

        astm := stm.to_array ;
        assert astm.dimensions = 1
            report "FAIL: a_t dimensions" severity failure ;
        assert astm.left = index(-1)
            report "FAIL: a_t left bound" severity failure ;
        assert astm.right = index(1)
            report "FAIL: a_t right bound" severity failure ;
        assert astm.ascending
            report "FAIL: a_t ascending" severity failure ;
        assert astm.length = index(3)
            report "FAIL: a_t length" severity failure ;

        -- b_t: unconstrained array -- should still reflect as CLASS_ARRAY.
        stm := b_t'reflect ;
        assert stm.get_type_class = CLASS_ARRAY
            report "FAIL: b_t class" severity failure ;

        -- c_t: constrained 1-D array using a plain integer range.
        stm := c_t'reflect ;
        assert stm.get_type_class = CLASS_ARRAY
            report "FAIL: c_t class" severity failure ;
        astm := stm.to_array ;
        assert astm.dimensions = 1
            report "FAIL: c_t dimensions" severity failure ;
        assert astm.left = index(-1)
            report "FAIL: c_t left bound" severity failure ;
        assert astm.right = index(1)
            report "FAIL: c_t right bound" severity failure ;
        assert astm.ascending
            report "FAIL: c_t ascending" severity failure ;
        assert astm.length = index(3)
            report "FAIL: c_t length" severity failure ;

        report "All reflection checks passed" ;
        std.env.stop ;
    end process ;

end architecture ;
