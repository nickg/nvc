architecture a of b is
    type my_int is range 0 to 100;
    signal x : my_int := 2;

    type resistance is range 0 to 10000000
        units
            ohm;
            kohm = 1000 ohm;
            Mohm = 1000 kohm;
        end units;
    signal r : resistance := 100 ohm;
begin

end architecture;
