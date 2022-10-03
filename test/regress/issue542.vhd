entity issue542 is
end entity;

architecture beh of issue542 is
    signal s : bit_vector(1 to 3);
begin

  p_proc : process
    procedure proc(
      signal   target        : inout  bit_vector) is
    begin
      target(target'range) <= (others => '0'); -- Issue is here
    end;
  begin
    s <= "111";
    wait for 1 ns;
    proc(s);
    assert s = "111";
    wait for 0 ns;
    assert s = "000";
    wait;
  end process;
end architecture;
