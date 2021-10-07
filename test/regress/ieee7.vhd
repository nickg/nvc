entity ieee7 is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.fixed_pkg.all;

architecture test of ieee7 is
begin

    main: process is
        variable x, y, z : ufixed(7 downto -3) := (others => '0');
    begin
        z := to_ufixed(0, 7, -3);
        assert x + y = z;
        x := to_ufixed(5, 7, -3);
        y := to_ufixed(6, 7, -3);
        z := to_ufixed(11, 7, -3);
        assert x + y = z;
        assert to_string(z) = "00001011.000" report to_string(z);
        x := to_ufixed(7, 7, -3);
        y := to_ufixed(2, 7, -3);
        z := to_ufixed(3.5, 7, -3);
        assert x / y = z;
        assert to_string(z) = "00000011.100" report to_string(z);
        wait;
    end process;

end architecture;
