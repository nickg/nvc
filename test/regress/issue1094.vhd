entity A is
  port (din : BIT_VECTOR(4 downto 0));
end A;

architecture arch of A is
begin
    check: process is
    begin
        assert din = "11110" report to_string(din)
            severity warning;  -- XXXX: wrong!
        wait for 1 ns;
        assert din = "11101" report to_string(din)
            severity warning;  -- XXXX: wrong!
        wait;
    end process;
end architecture;

package pkg_B is
  function init return natural;
end pkg_B;

package body pkg_B is
  function init return natural is
  begin
    return 1;
  end init;
end pkg_B;

use work.pkg_B.init;

package pkg_C is
	type bar is record
		field  : BIT_VECTOR(init downto 0);
	end record;
end package pkg_C;

use work.pkg_C.bar;

entity issue1094 is
end entity issue1094;

architecture rtl of issue1094 is

  signal foo : bar := ( field => "10" );

  function bar_to_bitvector(bar_obj: bar) return BIT_VECTOR is
      variable r : bit_vector(4 downto 0) := (others => '1');
  begin
      r(bar_obj.field'length - 1 downto 0) := bar_obj.field;
      return r;
  end;

begin

  test_inst: entity work.A
      port map(din => bar_to_bitvector(foo));

  foo.field <= "01" after 1 ns;
end architecture;
