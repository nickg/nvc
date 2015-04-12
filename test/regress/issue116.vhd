entity issue116 is
end issue116;

architecture behav of issue116 is

  signal intstat         : BIT_VECTOR (7 DOWNTO 0);
  ALIAS  INT_int            : BIT is intstat(7);

begin

    INT_int <= '1' when (intstat(6 downto 0) and "1111111") /= "0000000";
    --intstat(7) <= '1' when (intstat(6 downto 0) and "1111111") /= "0000000";

  process
  begin
    assert intstat(7) = '0';
    intstat(6 downto 0) <= "0000001";
    wait for 1 ns;
    assert INT_int = '1';
    intstat(6 downto 0) <= "0000000";
    wait for 1 ns;
    assert INT_int = '1';
    wait;
  end process;
end behav;
