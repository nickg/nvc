package test_pkg is
  type t_test is (GL, RTL);
end package;

use work.test_pkg.all;

entity test1 is
  generic (
    GC_TEST_TYPE : t_test
  );
end entity;

architecture rtl of test1 is
begin
  g_test : if GC_TEST_TYPE = RTL generate
    p_proc : process
    begin
      wait;
    end process;
  end generate g_test;
end architecture;

use work.test_pkg.all;

entity test is
end entity;

architecture rtl of test is
  signal sig  : bit;
begin
  i_test1 : work.test1
  generic map (GC_TEST_TYPE => RTL);
end architecture;
