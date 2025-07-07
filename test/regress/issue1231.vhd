entity issue1231 is
end entity issue1231;

architecture rtl of issue1231 is
    -- This function should shift a vector left or right by a specific amount.
    -- Due to a slice mismatch occurring in line 29, it instead returns some random characters??
    function expected_shift (
        input_vec : bit_vector;
        shift : integer
    ) return bit_vector is
        constant len : natural := input_vec'length;
        variable temp : bit_vector(len-1 downto 0);
        variable input_vector : bit_vector(len-1 downto 0) := input_vec;
        variable abs_shift : natural;
    begin
        abs_shift := abs(shift);

        if abs_shift >= len then
            temp := (others => '0');
        else
            if shift >= 0 then  -- Left shift
                temp := input_vec(len-1-abs_shift downto 0) & (abs_shift-1 downto 0 => '0');
            else   -- Right shift
                temp := (abs_shift-1 downto 0 => '0') & input_vec(len-1 downto abs_shift);
            end if;
        end if;
        return temp;
    end function;
begin
    MAGIC : process begin
        -- I assume the directly written bit_vector is a "to" instead of "downto" type, thus causing the mismatch
        report "Surprise 1 " & to_string(expected_shift("00000000", 0));
        report "Surprise 2 " & to_string(expected_shift("11111111", 0));
        report "Surprise 3 " & to_string(expected_shift("10101010", -1));
        report "Surprise 4 " & to_string(expected_shift("01010101", 1));
        -- but instead of failing with a loud bang, it results in weird, random characters with no warnings or errors.
        wait;
    end process;
end architecture;
