library ieee;
use ieee.std_logic_1164.all;

entity psl4 is
end entity psl4;

architecture psl of psl4 is

  signal oh  : std_logic_vector(3 downto 0) := "0001";
  signal iu  : std_logic_vector(3 downto 0) := "0000";

  signal co1 : std_logic_vector(7 downto 0) := "00000000";
  signal co2 : std_logic_vector(7 downto 0) := "00000001";
  signal co3 : std_logic_vector(7 downto 0) := "11111111";
  signal co4 : std_logic_vector(7 downto 0) := "1HHH0LLL";
  signal co5 : std_logic_vector(7 downto 0) := "10101010";

  signal clk : std_logic := '1';

  -- psl default clock is rising_edge(clk);

begin

  process
  begin
    wait for 5 ns;
    clk <= '0';
    wait for 5 ns;
    clk <= '1';
  end process;

  process
  begin

    ---------------------------------------------------------------------------
    -- Test "onehot" and "onehot0"
    ---------------------------------------------------------------------------
    wait until falling_edge(clk);
    oh  <= "0010";

    wait until falling_edge(clk);
    oh  <= "0100";

    wait until falling_edge(clk);
    oh  <= "1000";

    wait until falling_edge(clk);
    oh  <= "0000";

    wait until falling_edge(clk);
    oh  <= "1111";

    wait until falling_edge(clk);
    oh <= "1000";

    ---------------------------------------------------------------------------
    -- Test "isuknown"
    ---------------------------------------------------------------------------
    iu <= "000Z";
    wait until falling_edge(clk);
    iu <= "11X0";
    wait until falling_edge(clk);
    iu <= "10U0";
    wait until falling_edge(clk);
    iu <= "W011";
    wait until falling_edge(clk);
    iu <= "1-00";
    wait until falling_edge(clk);
    iu <= "HLHL";
    wait until falling_edge(clk);

    ---------------------------------------------------------------------------
    -- Test "countones"
    ---------------------------------------------------------------------------
    co5 <= "01010101";
    wait until falling_edge(clk);
    co5 <= "00000000";
    wait until falling_edge(clk);

    std.env.finish;
  end process;

  -- psl asrt_onehot : assert always (onehot(oh));
  -- psl asrt_onehot0 : assert always (onehot0(oh));

  -- psl asrt_is_unknwon : assert always (isunknown(iu) = false);

  -- psl asrt_count_ones_1 : assert always (countones(co1) = 0);
  -- psl asrt_count_ones_2 : assert always (countones(co2) = 1);
  -- psl asrt_count_ones_3 : assert always (countones(co3) = 8);
  -- psl asrt_count_ones_4 : assert always (countones(co4) = 4);

  -- psl asrt_count_ones_5 : assert always (countones(co5) = 4);

end architecture psl;
