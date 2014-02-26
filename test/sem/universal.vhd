entity e is
end entity;

architecture a of e is
begin

    process is
        variable r : real;
        variable i : integer;
    begin
        r := 1.5 * 2;                   -- OK
        r := r * 2;                     -- Error
        r := 6 * 5.15;                  -- OK
        r := i * 1.51;                  -- Error
        r := 62.3 / 6;                  -- OK
        r := 1.51 / i;                  -- Error
        wait;
    end process;

end architecture;
