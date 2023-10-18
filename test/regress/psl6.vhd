-- Adapted from: https://github.com/tmeissner/psl_with_ghdl/blob/master/src/psl_until.vhd
-- License: GPL-3.0-or-later

library ieee;
  use ieee.std_logic_1164.all;


entity psl6 is
end entity;


architecture psl of psl6 is
  constant seq_a : std_logic_vector := "01000100000";
  constant seq_b : std_logic_vector := "00110011110";
  constant seq_c : std_logic_vector := "00001000001";

  constant seq_d : std_logic_vector := "01000100000";
  constant seq_e : std_logic_vector := "00111011111";
  constant seq_f : std_logic_vector := "00001000001";

  constant seq_g : std_logic_vector := "01000000000";
  constant seq_h : std_logic_vector := "00000000000";
  constant seq_i : std_logic_vector := "00100000000";

  signal a, b, c : std_logic;
  signal d, e, f : std_logic;
  signal g, h, i : std_logic;
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
  ggen: g <= seq_g(clk);
  hgen: h <= seq_h(clk);
  igen: i <= seq_i(clk);

  -- This assertion holds
  -- psl UNTIL_0_a : assert always (a -> next (b until c));

  -- This assertion holds
  -- psl UNTIL_1_a : assert always (d -> next (e until f));

  -- This assertion holds
  -- psl UNTIL_2_a : assert always (g -> next (h until i));

  -- This assertion doesn't hold at cycle 4
  -- psl UNTIL_3_a : assert always (a -> next (b until_ c));

  -- This assertion holds
  -- psl UNTIL_4_a : assert always (d -> next (e until_ f));

  -- This assertion doesn't hold at cycle 2
  -- psl UNTIL_5_a : assert always (g -> next (h until_ i));

end architecture psl;
