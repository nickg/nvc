-- https://github.com/ghdl/ghdl/issues/1842
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library work;

package signal_pkg is

  type t_sigs is array (natural range <>) of std_logic_vector(7 downto 0);

  type t_signals is record
    dta: t_sigs(0 to 7);
  end record;

end signal_pkg;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library work;
use work.signal_pkg.all;

entity source is
  generic (
    instance_number : integer := 0);
  port (
    rst_n_i   : in  std_logic;
    clk_i : in  std_logic;
    outs : out  t_signals
    );
end entity source;

architecture sim of source is

  signal toggle : std_logic := '0';

begin  -- architecture rtl
  process(clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        outs.dta(instance_number) <= (others => '0');
      else
        toggle <= not toggle;
        if toggle='0' then
          outs.dta(instance_number) <= (others => '0');
        end if;
          outs.dta(instance_number) <= std_logic_vector(to_unsigned(instance_number + 2, 8));
      end if;
    end if;
  end process;
end architecture sim;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library work;
use work.signal_pkg.all;

entity elab26 is
end elab26;

architecture beh1 of elab26 is

  signal clk : std_logic := '0';
  signal rst_n : std_logic := '0';
  signal test : t_signals;

begin  -- beh1

  clk <= not clk after 10 ns when now < 50 ns;

  process
  begin
    rst_n <= '0';
    wait for 30 ns;
    rst_n <= '1';
    wait;
  end process;

  g1: for i in 0 to 7 generate
    source_1: entity work.source
      generic map (
        instance_number => i)
      port map (
        rst_n_i => rst_n,
        clk_i   => clk,
        outs    => test);
  end generate g1;

  check_p: process is
  begin
      wait for 50 ns;
      for i in 0 to 7 loop
          assert test.dta(i) = (0 to 7 => 'U');
      end loop;
      wait;
  end process;

end beh1;
