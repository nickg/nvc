library ieee;
use ieee.std_logic_1164.all;

entity ISSUE1641_CHILD is
  port (
    CLK : in  std_logic;
    D   : in  std_logic;
    Q   : out std_logic := '0'
  );
end entity;

architecture TEST of ISSUE1641_CHILD is
begin
  process (CLK)
  begin
    if rising_edge(CLK) then
      Q <= D;
    end if;
  end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity issue1641 is
  port (
    clk : in std_logic;
    q   : out std_logic
  );
end entity;

architecture test of issue1641 is
begin
  reg: entity work.issue1641_child
    port map (
      clk => clk,
      d   => '1',
      q   => q
    );
end architecture;
