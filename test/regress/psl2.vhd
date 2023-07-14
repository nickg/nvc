-- Adapted from: https://github.com/tmeissner/psl_with_ghdl/blob/master/src/psl_sere.vhd
-- License: GPL-3.0-or-later

library ieee;
  use ieee.std_logic_1164.all;


entity psl2 is
end entity psl2;


architecture psl of psl2 is

  constant seq_a : std_logic_vector := "110000";
  constant seq_b : std_logic_vector := "010000";

  signal a, b : std_logic;
  signal clk : natural;

  -- All is sensitive to rising edge of clk
  -- psl default clock is clk'event;

begin

  clkgen: clk <= clk + 1 after 1 ns when clk < 5;

  agen: a <= seq_a(clk);
  bgen: b <= seq_b(clk);

  -- This assertion holds
  -- psl SERE_0_a : assert {a};

  -- This assertion holds
  -- psl SERE_1_a : assert {a; a};

  -- This assertion holds
  -- psl SERE_2_a : assert {a; a and b};

  -- This assertion doesn't hold at cycle 2
  -- psl SERE_3_a : assert always {a; a};

end architecture psl;
