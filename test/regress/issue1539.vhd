library ieee;
use ieee.std_logic_1164.all;

package p is
  type array_t is array (integer range <>) of std_logic_vector;
end package;

library ieee;
use ieee.std_logic_1164.all;
use work.p.all;

entity inner is
  generic (
    DATA_WIDTH : integer;
    DEPTH      : integer
  );
  port (
    clk : in std_logic;
    din : in std_logic_vector(DATA_WIDTH-1 downto 0)
  );
end entity;

architecture rtl of inner is
  signal r : array_t(DEPTH-1 downto 0)(DATA_WIDTH-1 downto 0);
begin
  process(clk) begin
    if rising_edge(clk) then
      r <= r(r'high-1 downto 0) & din;  -- runtime fatal here
    end if;
  end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use work.p.all;

entity wrap is
  port (
    clk : in std_logic;
    din : in std_logic_vector  -- unconstrained
  );
end entity;

architecture rtl of wrap is
  signal s : std_logic_vector(din'range);
begin
  s <= din;
  -- BUG: both generics derived from s'length
  inst : entity work.inner
    generic map (DATA_WIDTH => s'length, DEPTH => 64/s'length)
    port map (clk => clk, din => s);
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity issue1539 is
end entity;

architecture rtl of issue1539 is
  signal clk : std_logic := '0';
  signal d   : std_logic_vector(31 downto 0) := (others => '0');
begin
  clk <= not clk after 5 ns;
  uut : entity work.wrap port map (clk => clk, din => d);
  process
  begin
      wait for 50 ns;
      std.env.finish;
  end process;
end architecture;
