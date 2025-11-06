library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dut is
    port (
        clk     : in  std_logic;
        reset_n : in  std_logic;
        a       : in  std_logic_vector(3 downto 0);
        sq_a2   : out std_logic_vector(7 downto 0);
        sq_aa   : out std_logic_vector(7 downto 0)
    );
end entity;

architecture rtl of dut is
    signal sa : integer := 0;
begin
    process(clk)
    begin
        if reset_n = '0' then
            sq_aa <= (others => '0');
            sq_a2 <= (others => '0');
            sa <= 0;
        elsif rising_edge(clk) then
            sa <= to_integer(signed(a));
            sq_a2 <= std_logic_vector(to_signed(sa**2, sq_a2'length));
            sq_aa <= std_logic_vector(to_signed(sa * sa, sq_aa'length));
        end if;
    end process;

    process(sq_aa)
    begin
        report "Self-multiply value: " & to_hstring(sq_aa);
    end process;

end architecture;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue1338 is
end entity;

architecture test of issue1338 is

    signal clk : std_logic := '0';

    signal a : std_logic_vector(3 downto 0) := "0000";
    signal reset_n :std_logic := '0';
begin

    process
    begin
        for i in 0 to 6 loop
            clk <= not clk;
            wait for 5 ns;
        end loop;
        wait;
    end process;

    process
    begin
        wait for 1 ns;
        reset_n <= '1';

        for i in 0 to 4 loop
            wait until falling_edge(clk);
            a <= std_logic_vector(unsigned(a) + 1);
        end loop;
        wait;
    end process;

    i_dut : entity work.dut
    port map(
        clk     => clk,
        reset_n => reset_n,
        a       => a,
        sq_a2   => open,
        sq_aa   => open
    );

end architecture;