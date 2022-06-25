entity gensub1 is
end entity;

architecture test of gensub1 is
    function adder generic (n : integer) (x : integer) return integer is
    begin
        return x + n;
    end function;

    function add1 is new adder generic map (1);

    signal s : integer;
begin

    p1: process is
    begin
        assert add1(1) = 2;

        s <= 5;
        wait for 1 ns;
        assert add1(s) = 6;

        wait;
    end process;

end architecture;
