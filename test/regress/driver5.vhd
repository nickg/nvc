entity driver5 is
end entity;

architecture test of driver5 is

    type int_vec is array (integer range <>) of integer;

    function resolved(x : int_vec) return integer is
    begin
        return x'length;
    end function;

    subtype rint is resolved integer;

    signal s : rint;

begin

    s <= 5;

    process is
    begin
        assert s = 2;
        wait for 0 ns;
        assert s = 2;
        s <= 4;
        wait for 1 ns;
        assert s = 2;
        wait;
    end process;

end architecture;
