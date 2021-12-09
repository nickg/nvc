package predef is
end package;

package body predef is

    function add1(x : integer) return integer is
    begin
        return x + 1;
    end function;

    -- Calls to predefined functions should always be folded
    constant c1 : integer := 1 + 2;
    constant c2 : boolean := true xor false;

    -- This should not be folded in analysis phase
    constant c3 : integer := add1(2);

end package body;
