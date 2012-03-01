entity operator3 is
end entity;

architecture test of operator3 is
    type int_array is array (integer range <>) of integer;
begin

    process is
        variable x : int_array(1 to 3);
    begin
        x := (1, 2, 3);
        assert x < (2, 2, 3);
        assert x > (0, 0, 0);
        assert x < (1, 2, 4);
        assert x < (1, 2, 3, 4);
        assert not (x < (1, 2));
        assert x <= (1, 2, 3);
        assert x >= (1, 2, 3);
        assert x >= (1, 1, 1);
        wait;
    end process;
    
end architecture;
