entity agg4 is
end entity;

architecture test of agg4 is
    type int_array is array (integer range <>) of integer;

    procedure fill(a : out int_array; x : in integer) is
        alias aa : int_array(1 to a'length) is a;
    begin
        aa := (1 to a'length => x);
    end procedure;

    procedure fill2(a : out int_array; y : in integer) is
    begin
        assert a'length = 1;
        a := (a'left => y);
    end procedure;
    
begin

    process is
        variable v : int_array(1 to 3);
        variable w : int_array(5 to 5);
    begin
        v := (1 to 3 => 7);
        assert v = (7, 7, 7);
        v := (1 => 6, 2 to 3 => 5);
        assert v = (6, 5, 5);
        fill(v, 8);
        assert v = (8, 8, 8);
        fill2(w, 6);
        assert w = (5 => 6);
        wait;
    end process;
    
end architecture;
