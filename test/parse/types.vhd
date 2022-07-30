entity b is
end entity;

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

    type my_int_vec is array (natural range <>) of my_int;

    function resolved (x : my_int_vec) return my_int;

    subtype rint is resolved my_int;

    type p is access my_int;

    type f is file of my_int;

    file f1 : f open READ_MODE is "foo";

    file f2 : f is "bar";

    file f3 : f;

    type r1 is record
        a : integer;
        b : integer;
        c : foo(1 to 5);
    end record;

    file f4 : f is out "bar";           -- VHDL-87 compat

    file f5 : f is in "bar";           -- VHDL-87 compat

    type r2 is record
        x : integer;
    end record r2;

    subtype my_real is real;
    subtype my_real2 is real range 0.0 to 10.0;

    constant var : some_array(1 to 3);  -- Error

begin

end architecture;
