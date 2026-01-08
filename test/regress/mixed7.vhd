entity mixed7 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of mixed7 is
    component sub is
        port (
            clk_in : in std_logic;
            din : in std_logic_vector(7 downto 0);
            dout : out std_logic_vector(7 downto 0) );
    end component;

    signal clk : std_logic := '1';
    signal din : std_logic_vector(7 downto 0) := X"00";
    signal dout : std_logic_vector(7 downto 0);
begin

    clkgen: clk <= not clk after 5 ns;

    uut: component sub
        port map (clk, din, dout);

    test: process is
    begin
        wait until falling_edge(clk);
        din <= X"ab";
        wait until rising_edge(clk);
        assert dout = X"UU" report to_hstring(dout);
        din <= X"cd";
        wait until rising_edge(clk);
        assert dout = X"ab" report to_hstring(dout);
        din <= X"ef";
        wait until rising_edge(clk);
        assert dout = X"cd" report to_hstring(dout);
        wait for 0 ns;
        wait for 0 ns;
        wait for 0 ns;
        wait for 0 ns;
        assert dout = X"cd" report to_hstring(dout);
        wait for 1 fs;
        assert dout = X"ef" report to_hstring(dout);

        std.env.finish;
    end process;

end architecture;
