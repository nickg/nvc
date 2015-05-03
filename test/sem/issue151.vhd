package type_pkg is
  type rec_t is record
    field : integer;
  end record;
end package;

use work.type_pkg.all;
package pkg1 is
  function fun(rec : rec_t) return integer;
  impure function ifun(rec : rec_t) return integer;
  procedure proc(variable rec : inout rec_t);
end package;

use work.type_pkg.all;
use work.pkg1.all;
package pkg2 is
  function fun(rec : rec_t) return integer;  -- OK
  impure function ifun(rec : rec_t) return integer;
  procedure proc(variable rec : inout rec_t);
end package;
