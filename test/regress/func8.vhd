entity func8 is
end entity;

architecture test of func8 is

    type real_vector is array (natural range <>) of real;

    function lookup(index : integer) return real is
        constant table : real_vector := (
            0.62, 61.62, 71.7, 17.25, 26.15, 651.6, 0.45, 5.761 );
    begin
        return table(index);
    end function;

begin

    process is
        variable x : integer;
    begin
        x := 0;
        wait for 0 ns;
        assert lookup(x) = 0.62;        -- Avoid constant folding
        x := 2;
        wait for 0 ns;
        assert lookup(x) = 71.7;
        wait;
    end process;

end architecture;
