entity default1 is
end entity;

architecture test of default1 is
    signal s : positive;
begin

    process is
        type mat2x2 is array (1 to 2, 1 to 2) of natural;
        type int_array10 is array (1 to 10) of positive;
        type int_array5 is array (5 to 7) of positive;
        
        variable x : integer;
        variable v : bit_vector(1 to 3);
        variable m : mat2x2;
        variable p : int_array10;
        variable q : int_array5;
    begin
        assert s = 1;
        assert x = integer'left;
        assert v = ('0', '0', '0');
        assert m = ((0, 0), (0, 0));
        assert p = (1 to 10 => 1);
        assert q = (5 to 7 => 1);
        assert p'left = 1;
        assert p'right = 10;
        assert q'left = 5;
        assert q'right = 7;
        assert q'length = 3;
        s <= 4;                         -- FIXME
        wait;
    end process;
    
end architecture;
