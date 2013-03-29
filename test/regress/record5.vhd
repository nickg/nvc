entity record5 is
end entity;

architecture test of record5 is

    type rec is record
        b : bit_vector(1 to 8);
        i : integer;
    end record;

    type rec_array is array (natural range <>) of rec;

    function reduce_or(x : bit_vector) return bit is
        variable r : bit := '0';
    begin
        for i in x'range loop
            r := r or x(i);
        end loop;
        return r;
    end function;

    function foo(a : rec_array) return bit is
    begin
        return reduce_or(a(0).b);
    end function;

begin

    process is
        variable ra : rec_array(0 to 1) := (
            ( b => X"05", i => 6 ),
            ( b => X"1a", i => 1 ) );
    begin
        assert foo(ra) = '1';
        wait;
    end process;

end architecture;
