package fixed_generic_pkg is
    generic (g : integer);

    type UNRESOLVED_ufixed is array (INTEGER range <>) of bit;

    subtype ufixed is UNRESOLVED_ufixed;

    function to_ufixed (
        arg                     : NATURAL)
        return UNRESOLVED_ufixed;

end package;

package body fixed_generic_pkg is

    function to_ufixed (
        arg                     : NATURAL)
        return UNRESOLVED_ufixed is
    begin
        return (1 to 1 => '0');
    end function;
end package body;

-------------------------------------------------------------------------------

package fixed_pkg is new work.fixed_generic_pkg generic map (4);

-------------------------------------------------------------------------------

package float_generic_pkg is
    generic (
        package fixed_pkg is new work.fixed_generic_pkg
                           generic map (<>) );

  use fixed_pkg.all;

  type UNRESOLVED_float is array (INTEGER range <>) of bit;

  subtype float is UNRESOLVED_float;

  subtype UNRESOLVED_float32 is UNRESOLVED_float (8 downto -23);
  alias U_float32 is UNRESOLVED_float32;
  subtype float32 is float (8 downto -23);

  function to_ufixed (
    arg                     : UNRESOLVED_float)
    return UNRESOLVED_ufixed;
end package;

package body float_generic_pkg is
  function to_ufixed (
    arg                     : UNRESOLVED_float)
      return UNRESOLVED_ufixed is
  begin
      return (1 to 1 => '0');
  end function;
end package body;

-------------------------------------------------------------------------------

package float_pkg is new work.float_generic_pkg generic map (work.fixed_pkg);

-------------------------------------------------------------------------------

entity genpack13 is
end entity;

use work.fixed_pkg.all;
use work.float_pkg.all;

architecture test of genpack13 is
begin

    main: process is
        subtype ufixed7 is ufixed (3 downto -3);   -- 7 bit
        variable checknum : float32;
        variable check7uf1, check7uf : ufixed7;
    begin
        check7uf1 := to_ufixed (checknum);
        wait;
    end process;

end architecture;
