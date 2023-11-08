package com_if_pkg is
  constant C_NOM_IDX : natural   := 1;
  constant C_RED_IDX : natural   := 2;
  type t_com_if   is (NOM, RED);
  type t_com_type is (UART);

  type t_com_interface is protected
    procedure set(constant value : t_com_if);
    impure function get return natural;
  end protected t_com_interface;
end package com_if_pkg;

use work.com_if_pkg.all;

package com_if_shared_pkg is
  shared variable protected_com_if : t_com_interface;
end package com_if_shared_pkg;

use work.com_if_pkg.all;
use work.com_if_shared_pkg.all;

package com_pkg is
  alias t_com_if is work.com_if_pkg.t_com_if;

  -- Crash here
  alias protected_com_if is work.com_if_shared_pkg.protected_com_if;
end package;
