entity e is
end entity;

architecture a of e is
begin

    process is
        variable r : real;
        variable i : integer;
    begin
        r := 1.5 * 2;                   -- Error
        r := r * 2;                     -- Error
        r := 6 * 5.15;                  -- Error
        r := i * 1.51;                  -- Error
        r := 62.3 / 6;                  -- Error
        r := 1.51 / i;                  -- Error
        r := real(1.5 * 2);             -- OK
        r := real(6 * 5.15);            -- OK
        r := real(62.3 / 6);            -- OK
        wait;
    end process;

end architecture;
