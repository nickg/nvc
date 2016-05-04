entity jcore1 is
    function add2(x : integer) return integer is
    begin
        return x + 2;
    end function;
end entity;

architecture test of jcore1 is
    signal x : integer := 4;
begin

    process is
    begin
        assert add2(x) = 6;
        wait;
    end process;

end architecture;
