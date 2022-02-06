entity ieee8 is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.float_pkg.all;

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
        variable a, b, c : float32;
    begin
        a := to_float(2.0);
        b := to_float(3.0);
        c := a + b;
        report to_string(to_real(c));
        check(c, 5.0);
        c := a * b;
        check(c, 6.0);
        wait;
    end process;

end architecture;
