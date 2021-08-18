entity bounds18 is
end entity;

architecture test of bounds18 is

    function func(x : bit_vector(1 to 5)) return bit is
    begin
        return x(1) and x(5);
    end function;

    procedure proc(n : positive) is
        variable v : bit_vector(1 to n);
    begin
        assert func(v) = '0';
    end procedure;

begin

    process is
        variable v : bit_vector(1 to 4);
    begin
        --assert func(v) = '0';         -- Caught during analysis
        proc(5);                        -- OK
        proc(3);                        -- Failure here
        wait;
    end process;

end architecture;
