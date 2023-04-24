library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity unaffected_ff is
  port (
    clk : in std_logic;
    sel : in std_logic_vector(1 downto 0);
    a, b : in std_logic;
    y: out std_logic
  );
end unaffected_ff;

architecture behavioral of unaffected_ff is
begin
P1 : process(clk)
begin
  if rising_edge(clk) then
    case to_integer(unsigned(sel)) is
      when 0 => y <= a;
      when 1 => y <= b;
      when others => y <= unaffected;
    end case;
  end if;
end process;
end architecture;

-------------------------------------------------------------------------------

entity issue677 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of issue677 is
    signal clk     : std_logic := '0';
    signal sel     : std_logic_vector(1 downto 0);
    signal a, b    : std_logic;
    signal y       : std_logic;
    signal running : boolean := true;
begin
    u: entity work.unaffected_ff
        port map (
            clk => clk,
            sel => sel,
            a   => a,
            b   => b,
            y   => y );

    clkgen: clk <= not clk after 5 ns when running else unaffected;

    stim: process is
    begin
        wait until falling_edge(clk);

        sel <= "00";
        a <= '1';
        b <= '0';
        wait until falling_edge(clk);
        assert y = '1';

        sel <= "01";
        wait until falling_edge(clk);
        assert y = '0';

        sel <= "10";
        wait until falling_edge(clk);
        assert y = '0';

        running <= false;
        wait;
    end process;
end architecture;
