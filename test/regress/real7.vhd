entity real7 is
end entity;

architecture test of real7 is
begin

    process is
        variable r1, r2, r3 : real;
        variable a1 : real_vector(0 to 1);
    begin
        a1 := (r1 * r1, 0.0);           -- Error
        r3 := a1(0) * 0.0;              -- Should not be NaN
        report real'image(r3);
        wait;
    end process;

end architecture;
