package prot_pkg is
  type t_prot is protected
    impure function get return natural;
  end protected t_prot;
end package prot_pkg;

package body prot_pkg is
  type t_prot is protected body
    impure function get return natural is
    begin
      return 1;
    end function get;
  end protected body t_prot;
end package body prot_pkg;
----------
use work.prot_pkg.all;
package shared_pkg is
  shared variable shared_var_prot : t_prot;
end package shared_pkg;
----------

package test_pkg is
  alias shared_var_prot is work.shared_pkg.shared_var_prot;
  constant C_CONST  : natural := shared_var_prot.get;
  constant C_CONST2 : natural := work.shared_pkg.shared_var_prot.get; -- Works
end package;
