entity issue1177 is
end entity;

architecture behave of issue1177 is

  signal test : bit := '0';
  --signal test_stable : boolean := false;

begin

 -- test_stable <= test'stable(50 ns);

  process
  begin
    test <= '0';
    wait for 10 ns;
    test <= '1';
    wait for 10 ns;
    test <= '0';
    wait for 10 ns;
    test <= '0';
    for i in 0 to 6 loop
      wait for 10 ns;
      if test'stable(50 ns) then
          assert now >= 70 ns;
      end if;
    end loop;
    assert test'stable(50 ns);
    wait;
  end process;

end architecture behave;
