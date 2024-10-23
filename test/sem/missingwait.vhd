entity foo is end entity;

architecture arch of foo is
  signal a : bit;

  procedure does_wait is
  begin
    wait for 10 ns;
  end procedure;

  procedure does_not_wait is
  begin
    report "not waiting";
  end procedure;

  procedure does_indirectly_wait is
  begin
    does_wait;
  end procedure;
begin

  -- Warning, contains no wait.
  process is
  begin
    report "no wait";
  end process;

  -- Ok, contains wait.
  process is
  begin
    wait for 10 ns;
  end process;

  -- Ok, has sensitivity.
  process (a) is
  begin
    report "a changed";
  end process;

  -- Ok, has sensitivity.
  process (ALL) is
  begin
    report "something changed";
  end process;

  -- Ok, calls procedure with wait.
  process is
  begin
    does_wait;
  end process;

  -- Warning, calls procedure without wait.
  process is
  begin
    does_not_wait;
  end process;

  -- Ok, calls procedure with indirect wait.
  process is
  begin
    does_indirectly_wait;
  end process;

  -- Ok, wait in if block.
  process is
  begin
    if now < 100 ns then
      wait for 1 ns;
    else
      wait;
    end if;
  end process;

  -- Ok, pcall with wait in if block.
  process is
  begin
    if now < 100 ns then
      does_wait;
    end if;
  end process;

  -- Ok, wait in case block.
  process is
  begin
    case a is
      when '0' =>
        wait for 1 ns;
      when '1' =>
        wait for 10 ns;
      when others =>
        wait on a;
    end case;
  end process;

  -- Ok, pcall with wait in case block.
  process is
  begin
    case a is
      when '0' =>
        does_wait;
      when '1' =>
        does_not_wait;
      when others =>
        null;
    end case;
  end process;

  -- Ok, calls procedure with indirect wait.
  process is
  begin
    does_indirectly_wait;
  end process;

end architecture;
