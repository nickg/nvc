entity issue69 is
end entity;

architecture test of issue69 is

    function flip(x : bit_vector) return bit_vector is
        variable r : bit_vector(x'reverse_range);
    begin
        for i in x'reverse_range loop
            r(i) := x(i);
        end loop;
        return r;
    end function;

begin

    process is
        variable v : bit_vector(1 to 4);
    begin
        v := "1100";
        assert flip(v) = "0011";
        v := "0101";
        assert flip(v) = "1010";
        wait;
    end process;

end architecture;
