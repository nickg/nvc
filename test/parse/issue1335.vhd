entity test is
end entity;

architecture beh of test is
    type t_data is array (natural range <>) of bit_vector(7 downto 0);
    signal data : t_data(0 to 7);
begin
    process is
    begin
        data <= (others => data(0)'subtype'(others => '0'));  -- OK
        wait;
    end process;
end architecture;
