entity default1 is
end entity;

architecture test of default1 is
    signal s : positive;
begin

    process is
        type mat2x2 is array (1 to 2, 1 to 2) of natural;
        
        variable x : integer;
        variable v : bit_vector(1 to 3);
        variable m : mat2x2;
    begin
        assert s = 1;
        assert x = integer'left;
        assert v = ('0', '0', '0');
        assert m = ((0, 0), (0, 0));
        s <= 4;                         -- FIXME
        wait;
    end process;
    
end architecture;
