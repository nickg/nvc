entity e is
end entity;

architecture a of e is
    signal x : integer := -3 * 4 + 2;
    type t is range -5 to 11 - 3;
    constant c : integer := +4 + 1;
    signal y : t;
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
    end process;

end architecture;
