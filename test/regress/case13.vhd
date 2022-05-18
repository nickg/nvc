entity case13 is
end entity;

architecture test of case13 is

    function get_bits (n : natural; b : bit) return bit_vector is
        variable v : bit_vector(1 to n);
    begin
        v := (others => b);
        return v;
    end function;

    signal n : natural := 3;
    signal b : bit;
begin

    p1: process (n, b) is
    begin
        case get_bits(n, b) is
            when "111" => report "ones";
            when "000" => report "zeros";
            when others => assert false;
        end case;
    end process;

    stim: process is
    begin
        b <= '1';
        wait for 0 ns;
        n <= 5;
        wait for 1 ns;
        assert false report "should not reach here!";
        wait;
    end process;

end architecture;
