package test_pkg is
  type t_test is (ONE, TWO);
end package;

package test2_pkg is
  alias t_test is work.test_pkg.t_test;
end package;

use work.test_pkg.all;
use work.test2_pkg.all;
entity test is
  generic(
     test_type : t_test := ONE
  );
end entity test;
