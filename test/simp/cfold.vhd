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
    end process;

end architecture;
