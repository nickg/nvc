entity e is
end entity;

architecture a of e is

    signal x : real := 1.234;           -- OK

    type my_real is range 0.0 to 1.0;   -- OK

begin

    process is
        variable v : my_real;
    begin
        x <= x + 6.1215;                -- OK
        x <= v;                         -- Error
    end process;

    process is
        variable i : integer;
    begin
        i := integer(x);                -- OK
        x <= real(i);                   -- OK
        x <= real(5);                   -- OK
        x <= real(bit'('1'));           -- Error
    end process;

    process is
        variable x : real;
    begin
        x := real'left;                 -- OK
        x := real'right;                -- OK
    end process;

    process is
        constant i : integer := 5;
        constant r : real := 252.4;
        type t is range r to i;         -- Error
        type t2 is range i to r;        -- Error
    begin
    end process;

    process is
        variable t : time;
        variable r : real;
    begin
        r := real(t / 1 ps) * 1.0;      -- OK
    end process;

end architecture;
