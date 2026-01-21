library ieee ;
    use ieee.std_logic_1164.all ;

entity issue1388 is
end entity ;

architecture arch of issue1388 is

    -- No direct calling convention so it needs to be a generic?
    function generic_reverse generic (
        type A is array(type is (<>)) of type is private
    ) parameter(x : A) return A is
        subtype idx is x'index ;
        variable rv     : x'subtype := x ;
        variable inidx  : idx := x'right ;
    begin
        -- null or single element arrays are the same as the original
        if idx'length < 2 then
            return rv ;
        end if ;

        -- fill in right -> left
        for outidx in x'range loop
            rv(inidx) := x(outidx) ;

            -- only increment if we aren't done
            -- note: /= not passed in via generics
            if inidx /= x'left then
                if x'ascending = true then
                    inidx := idx'pred(inidx) ;
                else
                    inidx := idx'succ(inidx) ;
                end if ;
            end if ;
        end loop ;

        return rv ;
    end function ;

    procedure generic_display generic (
        type t is array(type is (<>)) of type is private ;
        function to_string(x : t'element) return string is <>
    ) parameter (x : t) is
        use std.textio.all ;
        variable l : line ;
    begin
        swrite(l, "(") ;
        for idx in x'range loop
            write(l, to_string(idx) & "=>" & to_string(x(idx))) ;
            if idx /= x'high then
                swrite(l, ", ") ;
            end if ;
        end loop ;
        swrite(l, ")") ;
        writeline(output, l) ;
    end procedure ;

    -- Instantiate for the different array types
    function reverse is new generic_reverse generic map(A => integer_vector) ;
    function reverse is new generic_reverse generic map(A => std_logic_vector) ;

    -- Instantiate for the different array types
    procedure display is new generic_display generic map(t => integer_vector) ;
    procedure display is new generic_display generic map(t => std_logic_vector) ;

    -- Not a function but nvc still accepts it
--    function fdisplay is new generic_display generic map(t => integer_vector) ;

begin

    tb : process
        variable x : integer_vector := (1, 2, 3, 4, 5, 6, 7, 8) ;
        variable y : integer_vector := reverse(x) ;

        variable a : std_logic_vector(15 to 20) := ('X', '-', 'U', '0', '1', 'L') ;
        variable b : a'subtype := reverse(a) ;

        variable c : std_logic_vector(9 to 9) := (9 => '1') ;
        variable d : c'subtype := reverse(c) ;

        -- variable n : std_logic_vector(1 to 0) ;
        -- variable m : n'subtype := reverse(n) ;

        variable p : integer_vector(4 to 5) := (1, 2) ;
        variable q : p'subtype := reverse(p) ;
    begin
        display(y) ;
        display(b) ;
        display(d) ;
--        display(m) ;
        display(q) ;
        std.env.stop ;
    end process ;

end architecture ;
