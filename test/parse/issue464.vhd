entity test is
end entity test;
architecture beh of test is
  type t_alert_level is (NO_ALERT, NOTE, TB_NOTE, WARNING, TB_WARNING, MANUAL_CHECK, ERROR, TB_ERROR, FAILURE, TB_FAILURE);
  function f_test
    return boolean
  is
  begin
    for i in NOTE to t_alert_level'right loop
      null;
    end loop;
    return false;
  end function;

  type t1 is (a,b);
  type t2 is (b,c);
  type t3 is (c,d);
BEGIN
  -- Was vests tc1551
  TESTING: PROCESS
    variable k : integer := 0;
  BEGIN
    for i in c downto b loop
      k := 5;
    end loop;
  end process;
end architecture beh;
