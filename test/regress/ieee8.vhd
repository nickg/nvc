entity ieee8 is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.float_pkg.all;
use ieee.fixed_pkg.all;

architecture test of ieee8 is

    procedure check(value : in float32; expect : in real) is
        variable r : real;
    begin
        r := to_real(value);
        assert value > expect - 0.00001;
        assert value < expect + 0.00001;
    end procedure;

begin

    main: process is
        subtype ufixed7 is ufixed (3 downto -3);   -- 7 bit
        variable a, b, c : float32;
        variable checknum : float32;
        variable check7uf1, check7uf : ufixed7;
    begin
        a := to_float(2.0);
        b := to_float(3.0);
        c := a + b;
        report to_string(to_real(c));
        check(c, 5.0);
        c := a * b;
        check(c, 6.0);

        checknum  := "00000000000000000000000000000000";   -- 0
        check7uf1 := to_ufixed (checknum, check7uf1'high, check7uf1'low);
        check7uf  := "0000000";
        wait;
    end process;

end architecture;
