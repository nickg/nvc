entity issue1494 is
end entity;

architecture test of issue1494 is
    type vec_array_t is array (natural range <>) of bit_vector(1 downto 0);
    constant vals : vec_array_t(0 to 2) := ("00", "01", "10");

    signal a : bit := '0';
    signal b : bit := '0';
    signal c : bit := '0';
begin

    process
    begin
        for i in vals'range loop
            case vals(i) is
                when "00" =>
                    a <= '1';
                when "01" =>
                    b <= '1';
                when others =>
                    c <= '1';
            end case;

            wait for 1 ns;
        end loop;

        wait;
    end process;

end architecture;
