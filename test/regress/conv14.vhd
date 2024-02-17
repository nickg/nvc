entity conv14 is
end entity;

architecture test of conv14 is
    type t_bv_array is array (natural range <>) of bit_vector;

    function "not" (x : t_bv_array) return t_bv_array is
        variable r : x'subtype;
    begin
        for i in x'range loop
            for j in x(x'left)'range loop
                r(i)(j) := not x(i)(j);
            end loop;
        end loop;
        return r;
    end function;

    signal s : t_bv_array(1 to 2)(1 to 2);

begin

    b: block is
        port ( p : t_bv_array(1 to 2)(1 to 2) );
        port map ( p => "not"(s) );
    begin

        check: process is
        begin
            assert p = ("11", "11");
            wait for 0 ns;
            assert p = ("01", "10");
            wait;
        end process;

    end block;

    stim: process is
    begin
        s <= ("10", "01");
        wait;
    end process;

end architecture;
