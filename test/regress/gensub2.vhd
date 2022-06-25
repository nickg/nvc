entity gensub2 is
end entity;

architecture test of gensub2 is
    function adder generic (type t;
                            function "+"(l, r : t) return t is <>;
                            n : t) (x : t) return t is
    begin
        return x + n;
    end function;

    function add1 is new adder generic map (t => integer, n => 1);
    function add1 is new adder generic map (t => real, n => 1.0);

    signal s : integer;
    signal r : real;
begin

    p1: process is
    begin
        assert add1(1) = 2;
        assert add1(2.0) = 3.0;

        s <= 5;
        r <= 4.0;
        wait for 1 ns;
        assert add1(s) = 6;
        assert add1(r) = 5.0;

        wait;
    end process;

end architecture;
