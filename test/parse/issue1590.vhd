package bug2_name_pkg is
  type id_t is range integer'low to integer'high;
  constant BASE_ID : id_t := 0;

  impure function get_name(id : id_t := BASE_ID) return string;
end package;

package body bug2_name_pkg is
  impure function get_name(id : id_t := BASE_ID) return string is
  begin
    return "name";
  end function;
end package body;

use work.bug2_name_pkg.all;

entity bug2_defaulted_function_returning_string_positional_actual is
end entity;

architecture test of bug2_defaulted_function_returning_string_positional_actual is
    type line is access string;
    procedure write (variable l : line; str : string) is
    begin
    end procedure;
begin
  process
    variable l  : line;
    variable id : id_t := BASE_ID;
  begin
    write(l, "prefix " & get_name(id));
    wait;
  end process;
end architecture;
