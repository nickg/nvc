package myfixed is
    generic ( whole : natural; frac : natural );

    constant width : natural := whole + frac;

    type fixed_t is array (1 to width) of bit;

    function "+"(x, y : fixed_t) return fixed_t;

    function total_bits return natural;

end package;

package body myfixed is

    function total_bits return natural is
    begin
        return whole + frac;
    end function;

    function "+"(x, y : fixed_t) return fixed_t is
        variable result : fixed_t;
    begin
        for i in 1 to width loop
            result(i) := x(i) or y(i);
        end loop;
        return result;
    end function;

end package body;

-------------------------------------------------------------------------------

entity genpack1 is
end entity;

architecture test of genpack1 is
    package myfixed_2_4 is new work.myfixed generic map (2, 4);
    use myfixed_2_4.all;

    constant c : natural := myfixed_2_4.width;
    constant d : myfixed_2_4.fixed_t := (others => '0');
begin
    p1: process is
        variable v : myfixed_2_4.fixed_t;
    begin
        assert c = 6;
        assert myfixed_2_4.total_bits = 6;
        v := "110000";
        v := v + d;
        assert v = "110000";
        assert whole = 2;
        assert frac = 4;
        wait;
    end process;

end architecture;
