entity issue481 is
end entity;

architecture beh of issue481 is
  signal sig_1 : bit;
begin
  process
  begin
    assert sig_1 = '0';
    sig_1 <= force '1';
    wait for 1 ps;
    assert sig_1 = '1' report "signal val is not 1 " & to_string(sig_1) severity failure;
    wait;
  end process;
end architecture beh;
