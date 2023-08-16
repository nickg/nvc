-------------------------------------------------------------------------------
--  Copyright (C) 2022  Nick Gasson
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU Lesser General Public License as
--  published by the Free Software Foundation; either version 3 of the
--  License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
--  You should have received a copy of the GNU Lesser General Public License
--  along with this program; if not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

package body ieee_support is

    procedure check_match_expression (val : std_ulogic_vector) is
    begin
        for i in val'range loop
            assert val(i) /= '-' report "value of matching case statement expression """
                & to_string(val) & """ contains a '-'";
        end loop;
    end procedure;

    procedure check_match_expression (val : std_ulogic) is
    begin
        assert val /= '-' report "value of matching case statement expression is '-'";
    end procedure;

  -- The standard specifies the matching relational operators on
  -- STD_ULOGIC are predefined but they are implemented here in VHDL for
  -- convenience. The compiler emits calls to these functions when
  -- lowering the predefined operators.

  type match_table_t is array (std_ulogic, std_ulogic) of std_ulogic;

  constant match_eq_table : match_table_t := (
      ( 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U', '1' ),
      ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', '1' ),
      ( 'U', 'X', '1', '0', 'X', 'X', '1', '0', '1' ),
      ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', '1' ),
      ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', '1' ),
      ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', '1' ),
      ( 'U', 'X', '1', '0', 'X', 'X', '1', '0', '1' ),
      ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', '1' ),
      ( '1', '1', '1', '1', '1', '1', '1', '1', '1' ) );

  function rel_match_eq (l, r : std_ulogic) return std_ulogic is
  begin
      return match_eq_table(l, r);
  end function;

  constant match_lt_table : match_table_t := (
      ( 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'X' ),
      ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ),
      ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', 'X' ),
      ( 'U', 'X', '0', '0', 'X', 'X', '0', '0', 'X' ),
      ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ),
      ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ),
      ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', 'X' ),
      ( 'U', 'X', '0', '0', 'X', 'X', '0', '0', 'X' ),
      ( 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ) );

  function rel_match_lt (l, r : std_ulogic) return std_ulogic is
  begin
      assert l /= '-' and r /= '-'
          report "STD_LOGIC_1164: '-' operand for matching ordering operator"
          severity ERROR;
      return match_lt_table(l, r);
  end function;

  function rel_match_leq (l, r : std_ulogic) return std_ulogic is
  begin
      assert l /= '-' and r /= '-'
          report "STD_LOGIC_1164: '-' operand for matching ordering operator"
          severity ERROR;
      return match_lt_table(l, r) or match_eq_table(l, r);
  end function;

end package body;
