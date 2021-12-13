entity func18 is
end entity;

architecture test of func18 is

    function get_array(n : in integer) return bit_vector is
        -- The state struct for this function cannot be allocated on the stack
        variable result : bit_vector(3 downto 0);

        function get_xor return bit_vector is
        begin
            case n is
                when 1 => return X"1";
                when 2 => return X"2";
                when others => return X"f";
            end case;
        end function;
    begin
        result := result xor get_xor;
        return result;
    end function;

begin

    p1: process is
        variable n : integer := 1;
    begin
        wait for 1 ns;
        assert get_array(n) = X"1";

        wait;
    end process;

end architecture;
