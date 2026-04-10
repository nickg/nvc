package p1 is
    type a is range -10 to 0 ;
end package;

package p2 is
    subtype a is character range 'z' downto 'a' ;
end package ;

entity range4 is
end entity ;

architecture arch of range4 is

    type ranges_t is array(natural range <>) of integer'range'record ;

    subtype a is integer range -10 to 0 ;
    subtype b is integer range 100 to 200 ;
    subtype c is integer range 4929 to 8388 ;
    subtype d is integer range 100 downto 3 ;

    constant set : ranges_t := (a'range'value, b'range'value, c'range'value) ;

    constant set2 : ranges_t := (
        ranges_t'element'(direction => ASCENDING, left => -10, right => 0),
        ranges_t'element'(direction => ASCENDING, left => 100, right => 200),
        ranges_t'element'(direction => ASCENDING, left => 4929, right => 8388)
    ) ;

    function in_range(x : in integer ; set : ranges_t) return boolean is
    begin
        for idx in set'range loop
            if set(idx).direction = ASCENDING then
                if x >= set(idx).left and x <= set(idx).right then
                    return true ;
                end if ;
            else
                if x >= set(idx).right and x <= set(idx).left then
                    return true ;
                end if ;
            end if ;
        end loop ;
        return false ;
    end function ;

    procedure test_non_const(l, r : integer) is
        subtype t_sub is integer range l to r;
        variable x : bit_vector(t_sub);
    begin
        assert x'range'value = (l, r, ascending) report to_string(x'range'value);
    end procedure;

    procedure test_unconstrained(x : bit_vector; l, r : integer) is
    begin
        assert x'range(1)'value = (l, r, ascending) report to_string(x'range'value);
    end procedure;

    alias a1 is work.p1.a ;

    alias a1rr is a1'range'record ;

    constant a1v : a1rr := a1'range'value ;

begin

    tb : process
        variable drv : d'range'record := d'range'value ;
        variable bv : bit_vector(1 to 5);
    begin
        assert in_range(100, set) report "in_range check failed" severity failure;
        assert d'range'value = drv report "equality operator failed" severity failure;
        assert d'range'value /= c'range'value report "inequality operator failed" severity failure ;
        assert ascending > descending = false report "> failed" severity failure;
        assert ascending = ascending report "= failed" severity failure ;
        assert ascending < descending report "< failed" severity failure ;
        assert ascending <= ascending report "<= failed" severity failure ;
        assert range_direction'range'value = (left => ascending, right => descending, direction => ascending) severity failure;
        test_non_const(5, 6);
        test_unconstrained(bv, 1, 5);
        std.env.stop ;
    end process ;

end architecture ;
