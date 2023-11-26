entity e is
end entity;

architecture a of e is
    signal x : integer := -3 * 4 + 2;
    type t is range -5 to 11 - 3;
    constant c : integer := +4 + 1;
    signal y : t;
    type int_array is array (integer range <>) of integer;
    constant a1 : int_array(1 to 5) := (1, 2, 3, 4, 5);
    constant a2 : int_array(1 to 7) := (2 to 3 => 6, others => 5);
    constant a3 : int_array(1 to 9) := (8 => 24, others => 0);
    constant a4 : int_array(5 downto 1) := (1, 2, 3, 4, 5);
    constant a5 : int_array(5 downto 1) := (5 downto 3 => -1, others => 1);
begin

    process is
        variable b : boolean;
        variable d : time;
    begin
        x <= c / 2;
        y <= t'high;
        y <= t'left;
        b := t'right = 8;
        b := (t'right - t'left) = 2;
        b := t'high /= 2;
        b := true and true;
        b := true and false;
        b := true or false;
        b := true xor true;
        b := not true;
        b := not false;
        b := true xnor false;
        b := false nand false;
        b := false nor true;
        b := 7 > 5 and 6 < 2;
        x <= a1(2);
        x <= a2(1);
        x <= a2(3);
        x <= a3(8);
        x <= a1'length;
        x <= a4(2);
        x <= a5(4);
        x <= 2 ** 4;
        d := -5 ns;
    end process;

    process is
    begin
        if true then
            x <= 1;
        end if;
        if false then
            x <= 5;
        end if;
        if false then
            null;
        else
            x <= 5;
        end if;
        while false loop
            null;
        end loop;
        if true then
            x <= 1;
            x <= 5;
            null;
        end if;
    end process;

    process is
        variable r : real;
        variable b : boolean;
    begin
        r := 1.0 + 0.0;
        r := 1.5 * 4.0;
        r := 2.0 / 2.0;
        b := 4.6 > 1.2;
    end process;

    process
        variable k  : time;
    begin
    end process;

    process
        type int2_vec is array (66 to 67) of integer;
        variable b : boolean;
    begin
        b := a1'length = 5;
        b := a1'low(1) = 1;
        b := a1'high(1) = 5;
        b := a1'left = 1;
        b := a1'right = 5;
        b := int2_vec'length = 2;
        b := int2_vec'low = 66;
    end process;

    process is
    begin
        case 1 is
            when 1 => null;
            when 2 => x <= 1;
            when others => report "bang";
        end case;
    end process;

    process is
        variable r : real;
    begin
        r := real(1.5 * 2);
        r := real(3 * 0.2);
        r := real(5.0 / 2);
        r := 2.0 ** 4;
    end process;

    process is
        constant one : bit := '1';
        variable b   : boolean;
    begin
        b := one = '1';
        b := '0' /= one;
    end process;

    -- Billowitch tc3170
    tc3170: process is
        constant L : REAL := -10.0;
        constant R : REAL := 10.0;
        type RT1 is range L to R;
    begin
        assert (  RT1'right = RT1(R)   );  -- Should be removed
    end process;

    bitvec: process is
        variable b : boolean;
    begin
        b := ("101" and "110") = "100";
        b := ("110" and "011") = "010";
        b := ("101" or "110") = "111";
        b := not "101" = "010";
        b := ("001" or "010") = "011";
        b := ("101" nor "110") = "000";
        b := ("101" nand "110") = "011";
        b := ("101" nor "110") = "000";
    end process;

end architecture;
