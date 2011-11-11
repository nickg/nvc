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

    subtype big_r is resistance range 1000 to 2000;

    subtype my_small_int is my_int range 0 to 5;

    subtype foo is my_int range 2 to my_int'high;

    subtype rint is resolved my_int;
begin

end architecture;
