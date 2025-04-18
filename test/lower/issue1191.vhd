package test_pkg is
  generic (
    DEPTH : positive := 8
  );

  type t_test is protected
  end protected t_test;

end package test_pkg;

package body test_pkg is

  type t_test is protected body
  end protected body;

end package body;

-------------------------------------------------------------------------------

entity test is
end entity;

architecture sim of test is

  package test_pkg_inst is new work.test_pkg;

  shared variable s_test_pkg_inst : test_pkg_inst.t_test;

begin
end architecture sim;
