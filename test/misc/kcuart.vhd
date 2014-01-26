entity kcuart is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of kcuart is
    signal clk          : std_logic := '0';
    signal tx_data      : std_logic_vector(7 downto 0);
    signal tx_full      : std_logic;
    signal tx_wr        : std_logic := '0';
    signal uart_tx      : std_logic;
    signal en_16_x_baud : std_logic;
begin

    clk <= not clk after 5 ns;

    tx_i: entity work.uart_tx6
        port map (
            data_in      => tx_data,
            en_16_x_baud => en_16_x_baud,
            serial_out   => uart_tx,
            buffer_write => tx_wr,
            buffer_full  => tx_full,
            buffer_reset => '0',
            clk          => clk );

end architecture;
