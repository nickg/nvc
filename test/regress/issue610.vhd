package com_if_pkg is
  constant C_NOM_IDX : natural   := 1;
  constant C_RED_IDX : natural   := 2;
  type t_com_if   is (NOM, RED);
  type t_com_type is (UART);

  type t_com_interface is protected
    procedure set(constant value : t_com_if);
    impure function get return natural;
  end protected t_com_interface;

  function to_com(
    constant value : natural)
    return t_com_type;
end package com_if_pkg;

package body com_if_pkg is
  type t_com_interface is protected body
    variable priv_idx_val : natural := C_NOM_IDX;

    procedure set(
      constant value : t_com_if)is
    begin
      case value is
        when RED =>  priv_idx_val := C_RED_IDX;
        when NOM =>  priv_idx_val := C_NOM_IDX;
        when others => report "set(if) Unknown com interface" severity error;
      end case;
    end procedure set;
    impure function get return natural is
    begin
      return priv_idx_val;
    end function get;
  end protected body t_com_interface;

  function to_com(
    constant value : natural)
    return t_com_type is
  begin
    case value is
      when C_RED_IDX => return UART;
      when C_NOM_IDX => return UART;
      when others =>
        report "to_com(nat) Unknown com interface" severity error;
        return UART;
    end case;
  end function to_com;

end package body com_if_pkg;

use work.com_if_pkg.all;

package com_if_shared_pkg is
  shared variable protected_com_if : t_com_interface;
end package com_if_shared_pkg;

use work.com_if_pkg.all;
use work.com_if_shared_pkg.all;

package com_pkg is
  alias t_com_if is work.com_if_pkg.t_com_if;
  alias protected_com_if is work.com_if_shared_pkg.protected_com_if;

  procedure read_test(
    constant msg_id       : in    natural);

end package;

package body com_pkg is
  procedure read_test(
    constant msg_id       : in    natural
  ) is
    constant C_IDX        : natural    := protected_com_if.get;
    constant C_COM        : t_com_type := to_com(C_IDX);
  begin
    report to_string(C_IDX);
    case C_COM is
      when UART     => report "OK";
      when others   => report "read_test() Unknown com interface " & to_string(C_COM) severity error;
    end case;
  end procedure;

end package body;

use work.com_pkg.all;
use work.com_if_pkg.all;

entity issue610 is
end entity;

architecture beh of issue610 is
begin

  p_proc : process
  begin
    protected_com_if.set(NOM);
    read_test(1);
    wait;
  end process;
end architecture;
