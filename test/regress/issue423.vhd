entity issue423 is
end entity issue423;
architecture rtl of issue423 is
  signal sig : bit := '0';
begin

  process
  begin
    sig <= '0' after 1 ps;
    wait for 3 ps;
    assert sig'last_active = 3 ps - 1 ps report "Signal should have been active" severity warning;
    assert sig'last_active = 3 ps - 1 ps;
    wait;
  end process;
end architecture rtl;
