library ieee;
use ieee.math_real.all;

entity issue762 is end;

architecture test of issue762 is
	function to_cycles(x: real) return integer is begin
		return integer(ceil(80.0e6 * x));
	end;

	constant CYCLES: positive := to_cycles(120.0e-9);
	subtype delay_t is bit_vector(CYCLES - 1 downto 0);
	signal delay: delay_t;
begin
end architecture;
