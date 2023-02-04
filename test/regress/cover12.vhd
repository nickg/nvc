library ieee;
use ieee.std_logic_1164.all;

entity clk_gate is
    port (
        clk_in              : in    std_logic;
        clk_en              : in    std_logic;
        scan_enable         : in    std_logic;
        vector_input        : in    std_logic_vector(3 downto 0);
        clk_out             : out   std_logic
    );
end clk_gate;

architecture rtl of clk_gate is

    signal clk_en_q : std_logic;

begin

    -- Latch
    process(clk_in, scan_enable, clk_en)
    begin
        if (clk_in = '0') then
            clk_en_q <= (clk_en or scan_enable);
        end if;
    end process;

    clk_out <= clk_in and clk_en_q;

end architecture;


library ieee;
use ieee.std_logic_1164.all;

entity cover12 is
end entity;

architecture test of cover12 is

    signal clk                  : std_logic;
    signal my_clock_gate_signal : std_logic := '0';
    signal clk_gated            : std_logic;

begin

    i_clk_gate : entity work.clk_gate
    port map (
        clk_in              => clk,
        clk_en              => my_clock_gate_signal,
        scan_enable         => '0',
        vector_input        => "0000",
        clk_out             => clk_gated
    );

    clk_gen_proc : process
    begin
        for i in 0 to 10 loop
            clk <= '0';
            wait for 5 ns;
            clk <= '1';
            wait for 5 ns;
        end loop;
        wait;
    end process;

    test_ctrl_proc: process
    begin
        wait for 1 ns;
        wait until rising_edge(clk);

        wait for 1 ns;
        my_clock_gate_signal <= '1';
        wait until rising_edge(clk);

        wait until rising_edge(clk);
        my_clock_gate_signal <= '0';
        wait until rising_edge(clk);

        wait;
    end process;

end architecture;
