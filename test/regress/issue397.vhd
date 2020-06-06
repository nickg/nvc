library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity issue397 is
end issue397;

architecture behav of issue397 is
begin
	process
		variable i: integer;
		variable r: real;
	begin

        r := 0.9;
        i := integer(r);
        assert i = 1 report real'image(r) & " cast to integer gives " & integer'image(i);

        r := 0.3;
        i := integer(r);
        assert i = 0 report real'image(r) & " cast to integer gives " & integer'image(i);

        report real'image(log2(real(8)));
        r := floor(log2(real(8)));
        i := integer(r);
        assert i = 3 report "floor(log2(real(8))) cast to integer gives "
            & integer'image(i) severity warning;  -- TODO

        wait;
        end process;
end behav;
