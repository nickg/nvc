package protected_type_pkg is
  type protected_t is protected
    procedure proc;
  end protected;
end package;

package body protected_type_pkg is
  type protected_t is protected body
    procedure proc is
    begin
    end;
  end protected body;
end package body;

-------------------------------------------------------------------------------

use work.protected_type_pkg.protected_t;

entity e is
end entity;

architecture a of e is
  procedure proc(variable prot : inout protected_t) is
  begin
    prot.proc;                          -- Was undefined
  end;
begin

end architecture;

-------------------------------------------------------------------------------

use work.protected_type_pkg.protected_t;

package protected_user_pkg is
  procedure proc(variable prot : inout protected_t);
end package;

package body protected_user_pkg is
  procedure proc(variable prot : inout protected_t) is
  begin
    prot.proc;                          -- Was undefined
  end;
end package body;
