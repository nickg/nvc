entity issue73 is
end entity;

architecture test of issue73 is
    type ma_t is array (1 downto 0) of bit_vector(3 downto 0);
    signal x : ma_t;                    -- 0..7
    signal y : ma_t;                    -- 8..15
begin

    x(0)(1 downto 0) <= "00";
    x(0)(3 downto 2) <= "11";
    x(1)(0 downto 0) <= "0";
    x(1)(3 downto 1) <= "101";

    process (y) is
    begin
        for i in x'range loop
            y(i)(1 downto 0) <= "00";
            y(i)(3 downto 2) <= "11";
        end loop;
    end process;

end architecture;
