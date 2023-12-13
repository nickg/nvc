package bar is
	function gen_baz(l: positive) return bit_vector;
end;

package body bar is
	function gen_baz(l: positive) return bit_vector is begin
		return 10X"240";
	end;
end;

package qux is
	generic (
		X: positive;
		BAZ: bit_vector := work.bar.gen_baz(X)
	);

	subtype baz_t is bit_vector(BAZ'high downto BAZ'low);

	function get_qux(state: baz_t) return baz_t;
end;

package body qux is
	function get_qux(state: baz_t) return baz_t is begin
		return 10D"501";
	end;
end;

entity issue815 is end;

architecture test of issue815 is
	package pkg is new work.qux generic map (X => 10);
	signal s0: pkg.baz_t;
begin
	process begin
		s0 <= pkg.baz_t'value(10D"500");
		wait for 0 ns;
		assert s0 = "0111110100";
		s0 <= pkg.get_qux(s0);
		wait for 0 ns;
		assert s0 = "0111110101";
		wait;
	end process;
end;
