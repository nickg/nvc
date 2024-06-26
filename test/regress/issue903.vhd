entity issue903 is
end entity;

architecture test of issue903 is
    function compare (expected : real; result : real) return integer is
    begin
        if expected = result then
            return 6;
        end if;
        return 5;
    end function;

begin

    check: process is
        variable r : real;
    begin
        assert compare(1.0, r) = 5;
        wait;
    end process;

end architecture;
