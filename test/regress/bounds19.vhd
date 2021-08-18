entity bounds19 is
end entity;

architecture test of bounds19 is

    type bv2d is array (positive range <>, positive range <>) of bit;

    function func(x : bv2d(1 to 5, 1 to 3)) return bit is
    begin
        return x(1, 2) and x(5, 1);
    end function;

    procedure proc(x, y : positive) is
        variable v : bv2d(1 to x, 1 to y);
    begin
        assert func(v) = '0';
    end procedure;

begin

    process is
        variable v : bv2d(1 to 5, 1 to 2);
    begin
        --assert func(v) = '0';         -- Caught during analysis
        proc(5, 3);                     -- OK
        proc(5, 2);                     -- Failure here
        wait;
    end process;

end architecture;
