package std_logic_perf is
    procedure test_to_x01;
    procedure test_and;
    procedure test_or;
    procedure test_xor;
    procedure test_equal;
    procedure test_not_equal;
end package;

library ieee;
use ieee.std_logic_1164.all;

package body std_logic_perf is

    procedure test_to_x01 is
        constant ITERS : integer := 100;
        variable s     : std_logic_vector(31 downto 0);
    begin
        for i in 1 to ITERS loop
            s := "Z10-H01U10101L00Z10-H01U10101L00";
            s := to_x01(s);
            assert s = "X10X101X10101000X10X101X10101000";
        end loop;
    end procedure;

    procedure test_and is
        constant ITERS : integer := 100;
        variable x, y  : std_logic_vector(32 downto 0);
    begin
        y := '0' & X"12345678";
        for i in 1 to ITERS loop
            x(i rem 33) := '1';
            x := x and y;
        end loop;
    end procedure;

    procedure test_or is
        constant ITERS : integer := 100;
        variable x, y  : std_logic_vector(32 downto 0);
    begin
        y := '0' & X"12345678";
        for i in 1 to ITERS loop
            x(i rem 33) := '1';
            x := x or y;
        end loop;
    end procedure;

    procedure test_xor is
        constant ITERS : integer := 100;
        variable s     : std_logic_vector(32 downto 0) := (others => '0');
    begin
        for i in 1 to ITERS loop
            s(31 downto 0) := s(32 downto 1) xor X"deadbeef";
            s(32) := '1';
        end loop;
    end procedure;

    procedure test_equal is
        constant ITERS : integer := 500;
        variable x, y  : std_logic_vector(32 downto 0);
        variable count : natural;
    begin
        for i in 1 to ITERS loop
            if x = y then
                count := count + 1;
                x(i rem 32) := y(0);
            end if;
        end loop;
        assert count = ITERS;
    end procedure;

    procedure test_not_equal is
        constant ITERS : integer := 500;
        variable x, y  : std_logic_vector(6 downto 0);
        variable count : natural;
    begin
        for i in 1 to ITERS loop
            if x /= y then
                count := count + 1;
                x(i rem 32) := y(0);
            end if;
        end loop;
        assert count = 0;
    end procedure;
end package body;
