entity issue1269 is
end entity;

architecture rtl of issue1269 is
    signal sample : natural := 0;
    signal arr : integer_vector(0 to 3);
begin

    gen_process : for i in 0 to 3 generate

        process (sample)
            variable result : natural;

            impure function add(input: natural) return natural is
            begin
                return sample + input;
            end function;

        begin
            result := add(i);
            report to_string(result);
            arr(i) <= result;
        end process;

    end generate;

    check: process is
    begin
        wait for 1 ns;
        assert arr = (0, 1, 2, 3);
        sample <= 5;
        wait for 1 ns;
        assert arr = (5, 6, 7, 8);
        wait;
    end process;

end architecture;
