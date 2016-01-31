package other_pkg is
  type rec_t is record
    field : integer;
  end record;
end package;

package body other_pkg is
end package body;

package pkg is
  type prot_t is protected
  end protected;
end package;

-- Uncomment to make it work.
-- library library_name;

use work.other_pkg.all;

package body pkg is
  type prot_t is protected body
    variable rec : rec_t;
    procedure method is
    begin
      report integer'image(rec.field);
    end procedure;
  end protected body;
end package body;
