entity assign6 is
end entity;

architecture test of assign6 is
    signal x : bit := '1';
begin

    main: process is
        type int_ptr is access integer;
        type int_ptr_vec is array (natural range <>) of int_ptr;
        variable a, b : bit;
        variable p, q : int_ptr;
    begin
        wait for 1 ns;
        (a, b) := bit_vector'(x, not x);
        assert a = '1';
        assert b = '0';

        p := new integer'(5);
        (p, q) := int_ptr_vec'(q, p);
        assert q.all = 5;
        assert p = null;
        wait;
    end process;

end architecture;
