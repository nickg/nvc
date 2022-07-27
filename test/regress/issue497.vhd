entity test is
end entity;

architecture rtl of test is
  signal sig  : bit_vector(1 downto 0);
begin
end architecture;

entity issue497 is
end entity;

architecture rtl of issue497 is
begin
  i_test : entity work.test;

  p_proc : process
    alias sig    is <<signal i_test.sig : bit_vector>>;
  begin
    assert sig = "00";
    sig <= force "11";
    wait for 1 ns;
    assert sig = "11";
    sig <= release;
    wait for 1 ns;
    assert sig = "00";
    wait;
  end process;
end architecture;
