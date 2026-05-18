entity issue1541 is
end entity;

architecture test of issue1541 is
    function neg (r : real) return real is
    begin
        return 0.0 - r;
    end function;
begin

    process is
        variable x : real := 1.5;
    begin
        assert neg(1.0) = -1.0;
        wait for 1 ns;
        assert neg(x) = -1.5;
        wait;
    end process;

end architecture;
