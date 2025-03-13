PACKAGE test_pkg IS
    TYPE t_unconstraint IS RECORD
        a : bit_vector;
        b : bit_vector;
    END RECORD;

    FUNCTION from_slv(x : bit_vector; dwidth : integer) RETURN t_unconstraint;
    FUNCTION to_slv(x   : t_unconstraint) RETURN bit_vector;
END PACKAGE;
PACKAGE BODY test_pkg IS
    FUNCTION from_slv(x : bit_vector; dwidth : integer) RETURN t_unconstraint IS
        VARIABLE tmp : t_unconstraint(
        a(dwidth - 1 DOWNTO 0),
        b(dwidth - 1 DOWNTO 0)
        );
    BEGIN
        tmp.a := x(dwidth - 1 DOWNTO 0);
        tmp.b := x(2 * dwidth - 1 DOWNTO dwidth);
        RETURN tmp;
    END FUNCTION;
    FUNCTION to_slv(x : t_unconstraint) RETURN bit_vector IS BEGIN
        RETURN x.b & x.a;
    END FUNCTION;
END PACKAGE BODY;

--------------------------------------------------------
USE work.test_pkg.ALL;

ENTITY test IS
    PORT (
        ir_unconstraint : IN  t_unconstraint;
        or_unconstraint : OUT t_unconstraint
    );
END ENTITY test;
ARCHITECTURE rtl OF test IS
BEGIN
    or_unconstraint <= ir_unconstraint after 1 ns;
END ARCHITECTURE rtl;

--------------------------------------------------------

ENTITY slv IS
    GENERIC (
        DWIDTH : integer
    );
    PORT (
        iv_data : IN  bit_vector(DWIDTH*2 - 1 DOWNTO 0);
        ov_data : OUT bit_vector(DWIDTH*2 - 1 DOWNTO 0)
    );
END ENTITY slv;
ARCHITECTURE rtl OF slv IS
BEGIN
    ov_data <= NOT iv_data after 1 ns;
END ARCHITECTURE rtl;

--------------------------------------------------------

USE work.test_pkg.ALL;
use std.env.all;

ENTITY issue1171 IS
END ENTITY issue1171;
ARCHITECTURE sim OF issue1171 IS

    CONSTANT DWIDTH : integer := 8;

    SUBTYPE t_constraint IS t_unconstraint(
    a(DWIDTH - 1 DOWNTO 0),
    b(DWIDTH - 1 DOWNTO 0)
    );
    FUNCTION from_slv(x : bit_vector) RETURN t_constraint IS BEGIN
        RETURN from_slv(x, DWIDTH);
    END FUNCTION;

    SIGNAL test_to_slv : t_constraint;
    SIGNAL slv_to_test : bit_vector(DWIDTH*2 - 1 DOWNTO 0);

BEGIN

    dut1 : ENTITY work.test
        PORT MAP(
            ir_unconstraint => from_slv(slv_to_test),
            or_unconstraint => test_to_slv
        );

    dut2 : ENTITY work.slv
        GENERIC MAP(DWIDTH => DWIDTH)
        PORT MAP(
            iv_data => to_slv(test_to_slv),
            ov_data => slv_to_test
        );

    check: process is
    begin
        wait for 1 ns;
        finish;
        wait;
    end process;

END ARCHITECTURE sim;
