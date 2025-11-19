package h_pkg is
  function h return natural;
end package;

package body h_pkg is
function h return natural is
  begin
    return 8;
  end h;
end package body;

use work.h_pkg.all;
package A_pkg is
  constant C_t_bits : integer := h;
  subtype C_t is bit_vector(C_t_bits - 1 downto 0);
  -- subtype C_t is bit_vector(7 downto 0); -- works

  type B_t is record
    C : C_t;
  end record;
  type B_arr_t is array (0 to 0) of B_t;

  type D_t is record
    B_arr : B_arr_t;
  end record;
  type D_arr_t is array (0 to 1) of D_t;

  procedure p(
    constant e : D_arr_t;
    signal f : out bit_vector(7 downto 0));
end package;

package body A_pkg is
  procedure p(
    constant e : D_arr_t;
    signal f : out bit_vector(7 downto 0)
    ) is
  begin
    for i in e'reverse_range loop
      f <= e(i).B_arr(0).C;
      wait for 10 ns;
    end loop;
  end procedure;
end package body;

use work.A_pkg.all;
entity issue1348 is
end issue1348;

architecture sim of issue1348 is
  signal g : bit_vector(7 downto 0) := (others => '0');
begin
  a: process
    procedure p(e : D_arr_t) is
    begin
      p(e, g);
    end procedure;

    variable f : D_arr_t;

  begin
    wait for 10 ns;

    f(0) := (
      (B_arr =>
        (0 => (C => x"DC"))));

    f(1) := (
      (B_arr =>
        (0 => (C => x"AB"))));

    assert f(0).B_arr(0).C = "11011100";
    assert f(1).B_arr(0).C = "10101011";

    p(f);
    wait;
  end process;

  b: process
  begin
    wait for 30 ns;
    assert g = x"DC"
      severity failure;
    wait;
  end process;
end architecture;
