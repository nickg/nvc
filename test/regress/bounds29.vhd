entity bounds29 is
end entity;

architecture test of bounds29 is
    subtype my_real is real range 0.0 to 10.0;

    constant MATH_PI            : REAL := 3.14159_26535_89793_23846;
    subtype PRINCIPAL_VALUE is REAL range -MATH_PI to MATH_PI;

begin

    main: process is
        variable p1, p2 : principal_value;
        variable r : my_real;
    begin
        p1 := -0.78539815744037;        -- OK
        wait for 1 ns;
        p2 := p1;                       -- OK
        r := 5.0;                       -- OK
        wait for 1 ns;
        r := r * 2.0;                   -- OK
        wait for 1 ns;
        r := r + 0.00001;               -- Error
        wait;
    end process;

end architecture;
