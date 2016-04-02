entity dwl_lu is
    port    (
        fs:         in bit_vector (2 downto 1);
        a, b:           in bit_vector (3 downto 0);
        lu_out:     out bit_vector (3 downto 0);
        carryout:   out bit);
end dwl_lu;

architecture behavioral of dwl_lu is

begin
    process (a, b)
    begin
        lu_out <= (not a);
        carryout <= '0';
    end process;

    process is
    begin
        wait for 5 fs;
    end process;

end behavioral;
