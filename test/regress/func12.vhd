entity func12 is
end entity;

architecture test of func12 is

    function popcnt_high(value : in bit_vector(7 downto 0)) return natural is
        variable cnt : natural := 0;
    begin
        report integer'image(value'left);
        for i in 7 downto 4 loop
            report bit'image(value(i));
            if value(i) = '1' then
                cnt := cnt + 1;
            end if;
        end loop;
        return cnt;
    end function;

    function get_bits(v : in bit_vector(7 downto 0)) return bit_vector is
    begin
        for i in v'range loop
            report integer'image(i) & " = " & bit'image(v(i));
        end loop;
        return v;
    end function;

begin

    process is
        variable v : bit_vector(0 to 7) := X"05";
    begin
        assert popcnt_high(v) = 0;
        v := X"f0";
        assert popcnt_high(v) = 4;
        assert popcnt_high(get_bits(X"20")) = 1;
        --assert popcnt_high(v(0 to 3)) = 2;
        wait;
    end process;

end architecture;
