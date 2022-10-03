entity issue540 is
end entity;
architecture beh of issue540 is
  procedure test(
    round_time   : time) is
    variable v_overshoot   : time    := now rem round_time;
  begin
    wait for (round_time - v_overshoot);
  end;
begin

  process is
  begin
    wait for 7 ns;
    test(10 ns);
    assert now = 10 ns;
    assert 5 ns mod 2 ns = 1 ns;
    wait;
  end process;

end architecture beh;
