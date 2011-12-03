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
begin

    process is
        variable b : boolean;
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
        x <= a3(10);                    -- Error!
        x <= a3(-1);                    -- Error!
        x <= a1'length;
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

end architecture;
