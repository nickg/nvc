-- Test case from Brian Padalino
library ieee ;
    use ieee.std_logic_1164.all ;

entity issue970 is
end entity ;

architecture arch of issue970 is

    -- note: can't get this to parse with rivieria
    --function reverse parameter(x : array(type is (<>)) of type is private) return x'subtype;

    -- naive generic reverse algorithm
    function reverse generic (
        type t is array(type is (<>)) of type is private
    ) parameter(x : t) return t is
        subtype idx is x'index ;
        variable rv     : x'subtype := x ;
        variable inidx  : idx := x'right ;
    begin
        -- null or single element arrays are the same as the original
        if x'length < 2 then
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

    procedure display generic (
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

begin

    tb : process
        variable x : integer_vector := (1, 2, 3, 4, 5, 6, 7, 8) ;
        variable y : integer_vector := reverse generic map(x'subtype)(x) ;

        variable a : std_logic_vector(15 to 20) := ('X', '-', 'U', '0', '1', 'L') ;
        variable b : a'subtype := reverse generic map(a'subtype)(a) ;

        variable c : std_logic_vector(9 to 9) := (9 => '1') ;
        variable d : c'subtype := reverse generic map(c'subtype)(c) ;

        variable n : std_logic_vector(1 to 0) ;
        variable m : n'subtype := reverse generic map(n'subtype)(n) ;

        variable p : integer_vector(4 to 5) := (1, 2) ;
        variable q : p'subtype := reverse generic map(p'subtype)(p) ;
    begin
        display generic map (y'subtype)(y) ;
        display generic map (b'subtype)(b) ;
        display generic map (d'subtype)(d) ;
        display generic map (m'subtype)(m) ;
        display generic map (q'subtype)(q) ;
        std.env.stop ;
    end process ;

end architecture ;
