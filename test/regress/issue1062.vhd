entity nvc_crash is
end entity nvc_crash;

architecture rtl of nvc_crash is
  signal test_signal : natural;
begin
    test_signal <= 1 after 1 ns, 2 after 2 ns;
end architecture rtl;

entity issue1062 is
  generic(
    ENABLE_EXTERNAL_NAME : boolean := true
  );
end entity;

architecture rtl of issue1062 is
  signal test_signal : natural;
begin

  g_external_name : if ENABLE_EXTERNAL_NAME generate
  begin
    test_signal <= <<signal .issue1062.i_nvc_crash.test_signal : natural>>;
  end generate g_external_name;

  i_nvc_crash : entity work.nvc_crash;

  check: process is
  begin
      wait for 1 ns;
      assert test_signal = 0;
      wait for 0 ns;
      assert test_signal = 1;
      wait for 5 ns;
      assert test_signal = 2;

      wait;
  end process;

end architecture rtl;
