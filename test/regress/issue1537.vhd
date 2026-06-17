entity test1 is
  generic (
    GC_CLK_PERIOD_32M : time
  );
end entity;

architecture rtl of test1 is
  signal rx_en     : bit;
begin
  p_proc : process is
    constant C_STABLE_REQUIREMENT : time := GC_CLK_PERIOD_32M;
  begin
    assert rx_en'stable(C_STABLE_REQUIREMENT);
    rx_en <= '1';
    wait for 20 ns;
    assert not rx_en'stable(C_STABLE_REQUIREMENT);
    wait for 20 ns;
    assert rx_en'stable(C_STABLE_REQUIREMENT);
    report "end-test";
    wait;
  end process;
end architecture;

package test_pkg is
  constant C_CLK_32M_PERIOD : time := 1 ns * 31.25;
end test_pkg;

use work.test_pkg.all;

--hdlregression:tb
entity issue1537 is
end entity;

architecture rtl of issue1537 is

begin
  i_test : entity work.test1(rtl)
  generic map(
    GC_CLK_PERIOD_32M => C_CLK_32M_PERIOD
  );
end architecture;
