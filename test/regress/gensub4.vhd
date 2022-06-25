package genfact is
    function fact generic (type t;
                           function "*"(l, r : t) return t is <>;
                           function "-"(l, r : t) return t is <>;
                           function "<"(l, r : t) return boolean is <>;
                           one : t) (n : t) return t;
end package;

package body genfact is

    function fact generic (type t;
                           function "*"(l, r : t) return t is <>;
                           function "-"(l, r : t) return t is <>;
                           function "<"(l, r : t) return boolean is <>;
                           one : t) (n : t) return t is
    begin
        if n < one then
            return one;
        else
            return n * fact(n - ONE);
        end if;
    end function;

end package body;

-------------------------------------------------------------------------------

entity gensub4 is
end entity;

architecture test of gensub4 is
    function fact_int is new work.genfact.fact
        generic map (t => integer, one => 1);

    function fact_real is new work.genfact.fact
        generic map (t => real, one => 1.0);

    signal s : integer;
    signal r : real;
begin

    p1: process is
    begin
        assert fact_int(1) = 1;
        assert fact_int(5) = 120;
        assert fact_real(1.0) = 1.0;
        assert fact_real(4.0) = 24.0;

        s <= 4;
        r <= 2.0;
        wait for 1 ns;
        assert fact_int(s) = 24;
        assert fact_real(r) = 2.0;

        wait;
    end process;

end architecture;
