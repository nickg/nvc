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
    begin
        assert lookup(0) = 0.62;
        assert lookup(2) = 71.7;
        wait;
    end process;

end architecture;
