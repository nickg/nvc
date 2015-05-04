package protected_type_pkg is
    type protected_t is protected
        impure function get_value return integer;
    end protected;
end package;

package body protected_type_pkg is
    type other_protected_t is protected
        impure function get_value return integer;
    end protected;

    type other_protected_t is protected body
      variable value : integer := 5;

      impure function get_value return integer is
      begin
        return value;
      end function;
    end protected body;

  type protected_t is protected body
    variable other_protected : other_protected_t;
    impure function get_value return integer is
    begin
      return other_protected.get_value;
    end function;
  end protected body;
end package body;

use work.protected_type_pkg.protected_t;

package pkg is
  shared variable prot : protected_t;
end package;

-------------------------------------------------------------------------------

entity issue141 is
end entity;

use work.pkg.all;

architecture test of issue141 is
begin

    process is
    begin
        assert prot.get_value = 5;
        wait;
    end process;

end architecture;
