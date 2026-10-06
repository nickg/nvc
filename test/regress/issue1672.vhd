library ieee;
use ieee.std_logic_1164.all;

entity issue1672 is
end entity;

architecture arch of issue1672 is
  procedure proc (
    signal src: in std_logic_vector (3 downto 0);
    signal dst: out std_logic_vector (3 downto 0)
  ) is
  begin
    dst <= src;
  end procedure;

  signal a : std_logic_vector (3 downto 0) := (others => '1');
  signal b : std_logic_vector (3 downto 0);

begin
  process (all)
  begin
    proc (src => a, dst (3 downto 0) => b);  -- Crashes
    --proc (src => a, dst => b);  -- Works
  end process;

  process
  begin
    assert b = "UUUU";
    wait for 0 ns;
    assert b = "1111";
    wait;
  end process;
end architecture;
