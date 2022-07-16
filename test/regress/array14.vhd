entity array14 is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use std.textio.all;

architecture test of array14 is

    -- Reduced from UVVM string_methods_pkg.vhd

  function pos_of_leftmost_non_zero(
    vector              : string;
    result_if_not_found : natural := 1
    ) return natural is
    alias a_vector : string(1 to vector'length) is vector;
  begin
    for i in a_vector'left to a_vector'right loop
      if (a_vector(i) /= '0' and a_vector(i) /= ' ') then
        return i;
      end if;
    end loop;
    return result_if_not_found;
  end;

  function adjust_leading_0(
    val     : string
    ) return string is
    alias    a_val      :  string(1 to val'length) is val;
    constant leftmost_non_zero : natural  := pos_of_leftmost_non_zero(a_val, 1);
  begin
    if val'length <= 1 then
      return val;
    end if;
        return a_val(leftmost_non_zero to val'length);
  end function;

  function to_string(
    val     : std_logic_vector) return string is
    variable v_line         : line;
    alias    a_val          : std_logic_vector(val'length - 1 downto 0) is val;
    variable v_result       : string(1 to 10 + 2 * val'length); --
    variable v_width        : natural;
    variable v_use_end_char : boolean := false;
  begin
    if val'length = 0 then
      -- Value length is zero,
      -- return empty string.
      return "";
    end if;

    write(v_line, adjust_leading_0(to_string(to_integer(unsigned(val)))));

    v_width := v_line'length;
    v_result(1 to v_width) := v_line.all;
    deallocate(v_line);
    return v_result(1 to v_width);
  end;

begin

    p1: process is
        variable v : std_logic_vector(1 to 8) := X"42";
    begin
        assert to_string(v) = "66";
        wait;
    end process;

end architecture;
