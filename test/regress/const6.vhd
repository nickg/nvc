entity const6 is
end entity;

architecture test of const6 is

    function min(x, y : integer) return integer is
    begin
        if x > y then
            return y;
        else
            return x;
        end if;
    end function;

    function get_left(x : bit_vector) return bit is
        constant l : integer := x'left;
        variable v : bit_vector(1 to x'right);
        constant m : integer := min(x'length, v'length) + 1;
    begin
        report integer'image(m);
        return x(l);
    end function;

begin

    process is
        variable x : bit_vector(3 downto 0) := "1010";
    begin
        wait for 0 ns;                  -- Prevent constant folding
        assert get_left(x) = '1';
        wait;
    end process;

end architecture;
