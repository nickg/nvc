-- Adapted from: https://github.com/tmeissner/psl_with_ghdl/blob/master/src/psl_next.vhd
-- License: GPL-3.0-or-later

library ieee;
  use ieee.std_logic_1164.all;


entity psl3 is
end entity psl3;


architecture psl of psl3 is

  constant seq_a : std_logic_vector := "01001100100";
  constant seq_b : std_logic_vector := "01100110011";

  constant seq_c : std_logic_vector := "01001100100";
  constant seq_d : std_logic_vector := "01100100011";

  signal a, b : std_logic;
  signal c, d : std_logic;
  signal clk : natural;

  -- All is sensitive to rising edge of clk
  default clock is clk'event;

begin

  clkgen: clk <= clk + 1 after 1 ns when clk < 10;

  agen: a <= seq_a(clk);
  bgen: b <= seq_b(clk);
  cgen: c <= seq_c(clk);
  dgen: d <= seq_d(clk);

  -- This assertion holds
  NEXT_0_a : assert always (a -> next b);

  -- This assertion doesn't hold at cycle 6
  NEXT_1_a : assert always (c -> next d);

end architecture psl;
