entity issue536 is
end entity;

architecture test of issue536 is
    subtype uint8_t is integer range 0 to 255;
    type uint8_array_t is array(natural range <>) of uint8_t;

    signal ttx_data  : uint8_array_t(0 to 127);
begin

    p1: process is
    begin
        wait for 1 ns;
        ttx_data(5) <= 2;
        wait for 5 ns;
        ttx_data(100) <= 55;
        wait;
    end process;

end architecture;
