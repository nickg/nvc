entity proc11 is
end entity;

architecture test of proc11 is

    function count_bits(x : in bit_vector) return natural is
        variable r : natural := 0;
    begin
        for i in x'range loop
            if x(i) = '1' then
                r := r + 1;
            end if;
        end loop;
        return r;
    end function;

    procedure proc(signal x : in bit_vector) is
    begin
        wait for 1 ns;
        assert count_bits(x) = 0;
        wait for 5 ns;
        assert count_bits(x) = 4;
    end procedure;

    signal s : bit_vector(1 to 8);

begin

    s <= X"aa" after 2 ns;

    proc(s);

end architecture;
