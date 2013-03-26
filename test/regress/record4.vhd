entity record4 is
end entity;

architecture test of record4 is

    type rec is record
        x, y : integer;
    end record;

    type rec_array is array (natural range <>) of rec;

    function sum(r : in rec) return integer is
    begin
        return r.x + r.y;
    end function;

    function double(x : in integer) return integer is
    begin
        return x * 2;
    end function;

begin

    process is
        variable ra : rec_array(0 to 1) := (
            ( 1, 2 ),
            ( 3, 4 ) );
    begin
        assert sum(ra(0)) = 3;
        assert double(ra(0).x) = 2;
        wait;
    end process;

end architecture;
