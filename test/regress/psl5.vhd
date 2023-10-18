-- Adapted from: https://github.com/tmeissner/psl_with_ghdl/blob/master/src/psl_next_3.vhd
-- License: GPL-3.0-or-later

library ieee;
  use ieee.std_logic_1164.all;


entity psl5 is
end entity psl5;


architecture psl of psl5 is

  constant seq_a : std_logic_vector := "00101000000";
  constant seq_b : std_logic_vector := "00000101000";

  constant seq_c : std_logic_vector := "00101000000";
  constant seq_d : std_logic_vector := "00000100000";

  constant seq_e : std_logic_vector := "00101000000";
  constant seq_f : std_logic_vector := "00000111110";

  signal a, b : std_logic;
  signal c, d : std_logic;
  signal e, f : std_logic;
  signal clk : natural;

  -- All is sensitive to rising edge of clk
  -- psl default clock is clk'event;

begin

  clkgen: clk <= clk + 1 after 1 ns when clk < 10;

  agen: a <= seq_a(clk);
  bgen: b <= seq_b(clk);
  cgen: c <= seq_c(clk);
  dgen: d <= seq_d(clk);
  egen: e <= seq_e(clk);
  fgen: f <= seq_f(clk);

  -- This assertion holds
  -- psl NEXT_0_a : assert always (a -> next[3] (b));

  -- This assertion doesn't hold at cycle 7
  -- psl NEXT_1_a : assert always (c -> next[3] (d));

  -- This assertion holds
  -- psl NEXT_2_a : assert always (e -> next[3] (f));

  -- This assertion holds
  -- psl dummy1: assert always (e -> next[0] (e));
  -- psl dummy2: assert always (e -> next[-1] (e));

end architecture psl;
