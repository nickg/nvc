use work.testtest_pkg.all;         -- Missing

package test2_pkg is
  type t_test is protected
    procedure test_proc;
  end protected;
end package;
package body test2_pkg is
  type t_test is protected body
    procedure test_proc is
      alias a_test_val is test_val.rec_val;
    begin
      a_test_val := 1;
    end procedure;
  end protected body;
end package body;
