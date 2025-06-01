package numeric_std_perf is
    procedure test_to_unsigned;
    procedure test_add_unsigned;
    procedure test_add_signed;
    procedure test_to_01;
    procedure test_resize;
    procedure test_to_signed;
    procedure test_mul_unsigned;
    procedure test_mul_signed;
    procedure test_sub_unsigned;
    procedure test_sub_signed;
    procedure test_add_one;
    procedure test_add_zero;
    procedure test_less_unsigned;
    procedure test_geq_unsigned;
    procedure test_div_unsigned;
    procedure test_big_div_unsigned;
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package body numeric_std_perf is

    procedure test_to_unsigned is
        constant WIDTH : integer := 8;
        constant ITERS : integer := 1;
        variable s     : unsigned(WIDTH - 1 downto 0);
    begin
        for i in 1 to ITERS loop
            for j in 0 to integer'(2 ** WIDTH - 1) loop
                s := to_unsigned(j, WIDTH);
            end loop;
        end loop;
    end procedure;

    procedure test_add_unsigned is
        constant WIDTH : integer := 16;
        constant ITERS : integer := 500;
        variable accum : unsigned(WIDTH - 1 downto 0) := (others => '0');
        constant one   : unsigned(WIDTH - 1 downto 0) := to_unsigned(1, WIDTH);
    begin
        for i in 1 to ITERS loop
            accum := accum + one;
        end loop;
        assert accum = to_unsigned(ITERS, WIDTH);
    end procedure;

    procedure test_add_signed is
        constant WIDTH : integer := 16;
        constant ITERS : integer := 500;
        variable accum : signed(WIDTH - 1 downto 0) := (others => '0');
        constant one   : signed(WIDTH - 1 downto 0) := to_signed(1, WIDTH);
    begin
        for i in 1 to ITERS loop
            accum := accum + one;
        end loop;
        assert accum = to_signed(ITERS, WIDTH);
    end procedure;

    procedure test_to_01 is
        constant ITERS : integer := 100;
        variable s     : unsigned(31 downto 0);
    begin
        for i in 1 to ITERS loop
            s := "Z10-H01U10101L00Z10-H01U10101L00";
            s := to_01(s, '0');
            assert s = (31 downto 0 => '0');
            s := "01010100100101000100101111100010";
            s := to_01(s, '-');
            assert s = "01010100100101000100101111100010";
        end loop;
    end procedure;

    procedure test_resize is
        constant ITERS : integer := 100;
        variable u1    : unsigned(31 downto 0);
        variable u2    : unsigned(39 downto 0);
        variable s1    : signed(31 downto 0);
        variable s2    : signed(7 downto 0);
    begin
        for i in 1 to ITERS loop
            u2 := resize(u1, 40);
            s2 := resize(s1, 8);
        end loop;
    end procedure;

    procedure test_to_signed is
        constant WIDTH : integer := 16;
        constant ITERS : integer := 1;
        variable s     : signed(WIDTH - 1 downto 0);
    begin
        for i in 1 to ITERS loop
            for j in 0 to integer'(2 ** (WIDTH/2) - 1) loop
                s := to_signed(j - 100, WIDTH);
            end loop;
        end loop;
    end procedure;

    procedure test_mul_unsigned is
        constant WIDTH : integer := 16;
        constant ITERS : integer := 15;
        constant two   : unsigned(WIDTH - 1 downto 0) := to_unsigned(2, WIDTH);
        variable accum : unsigned(WIDTH - 1 downto 0) := to_unsigned(1, WIDTH);
    begin
        for i in 1 to ITERS loop
            accum := resize(accum * two, WIDTH);
        end loop;
        assert accum = to_unsigned(2 ** ITERS, WIDTH);
    end procedure;

    procedure test_mul_signed is
        constant WIDTH : integer := 17;
        constant ITERS : integer := 15;
        constant two   : signed(WIDTH - 1 downto 0) := to_signed(2, WIDTH);
        variable accum : signed(WIDTH - 1 downto 0) := to_signed(1, WIDTH);
    begin
        for i in 1 to ITERS loop
            accum := resize(accum * two, WIDTH);
        end loop;
        assert accum = to_signed(2 ** ITERS, WIDTH);
    end procedure;

    procedure test_sub_unsigned is
        constant WIDTH : integer := 16;
        constant ITERS : integer := 500;
        variable accum : unsigned(WIDTH - 1 downto 0) := (others => '0');
        constant one   : unsigned(WIDTH - 1 downto 0) := to_unsigned(1, WIDTH);
    begin
        for i in 1 to ITERS loop
            accum := accum - one;
        end loop;
        assert accum = to_unsigned(2**WIDTH - ITERS, WIDTH);
    end procedure;

    procedure test_sub_signed is
        constant WIDTH : integer := 16;
        constant ITERS : integer := 500;
        variable accum : signed(WIDTH - 1 downto 0) := (others => '0');
        constant one   : signed(WIDTH - 1 downto 0) := to_signed(1, WIDTH);
    begin
        for i in 1 to ITERS loop
            accum := accum - one;
        end loop;
        assert accum = to_signed(-ITERS, WIDTH);
    end procedure;

    procedure test_add_one is
        constant WIDTH : integer := 16;
        constant ITERS : integer := 500;
        variable accum : unsigned(WIDTH - 1 downto 0) := (others => '0');
    begin
        for i in 1 to ITERS/2 loop
            accum := accum + 1;
        end loop;
        for i in 1 to ITERS/2 loop
            accum := 1 + accum;
        end loop;
        assert accum = to_unsigned(ITERS, WIDTH);
    end procedure;

    procedure test_add_zero is
        constant WIDTH : integer := 16;
        constant ITERS : integer := 500;
        variable accum : unsigned(WIDTH - 1 downto 0) := (others => '0');
    begin
        for i in 1 to ITERS/2 loop
            accum := accum + 0;
        end loop;
        for i in 1 to ITERS/2 loop
            accum := 0 + accum;
        end loop;
        assert accum = X"0";
    end procedure;

    procedure test_less_unsigned is
        constant WIDTH : integer := 16;
        constant ITERS : integer := 500;
        variable value : unsigned(WIDTH - 1 downto 0) := (others => '0');
        constant mid   : unsigned(WIDTH - 1 downto 0) := to_unsigned(ITERS / 2, WIDTH);
        constant one   : unsigned(WIDTH - 1 downto 0) := to_unsigned(1, WIDTH);
        variable accum : integer := 0;
    begin
        for i in 1 to ITERS loop
            if value < mid then
                accum := accum + 1;
            end if;
            value := value + one;
        end loop;
        assert accum = ITERS / 2;
    end procedure;

    procedure test_geq_unsigned is
        constant WIDTH : integer := 16;
        constant ITERS : integer := 500;
        variable value : unsigned(WIDTH - 1 downto 0) := (others => '0');
        constant mid   : unsigned(WIDTH - 1 downto 0) := to_unsigned(ITERS / 2, WIDTH);
        constant one   : unsigned(WIDTH - 1 downto 0) := to_unsigned(1, WIDTH);
        variable accum : integer := 0;
    begin
        for i in 1 to ITERS loop
            if value >= mid then
                accum := accum + 1;
            end if;
            value := value + one;
        end loop;
        assert accum = ITERS / 2;
    end procedure;

    procedure test_div_unsigned is
        constant WIDTH : integer := 16;
        constant ITERS : integer := 500;
        variable accum : unsigned(WIDTH - 1 downto 0) := (WIDTH - 1 => '1', others => '0');
        constant two   : unsigned(0 to WIDTH - 1) := to_unsigned(2, WIDTH);
    begin
        for i in 1 to ITERS loop
            accum := accum / two;
        end loop;
        assert accum = to_unsigned(0, WIDTH);
    end procedure;

    procedure test_big_div_unsigned is
        constant WIDTH : integer := 140;
        constant ITERS : integer := 500;
        variable accum : unsigned(WIDTH - 1 downto 0) := (WIDTH - 1 => '1', others => '0');
        constant two   : unsigned(0 to WIDTH - 1) := to_unsigned(2, WIDTH);
    begin
        for i in 1 to ITERS loop
            accum := accum / two;
        end loop;
        assert accum = to_unsigned(0, WIDTH);
    end procedure;
end package body;
