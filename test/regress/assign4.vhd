entity assign4 is
end entity;

architecture test of assign4 is
    type int_vec is array (natural range <>) of integer;
begin

    check: process is
        variable v : int_vec(1 to 2);
        variable a, b : integer;
    begin
        v := (1, 2);
        wait for 1 ns;
        (a, b) := v;
        wait for 1 ns;
        assert a = 1;
        assert b = 2;
        (a, b) := int_vec'(5, 7);
        assert a = 5;
        assert b = 7;
        wait;
    end process;

end architecture;
