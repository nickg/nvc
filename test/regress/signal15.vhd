-- VHDL 2008 element resolution
entity signal15 is
end entity;

architecture test of signal15 is
    type my_utype is (a, b, c);
    type my_utype_vector is array (natural range <>) of my_utype;
    type my_utype_vector_vector is array (natural range <>) of my_utype_vector(1 to 3);

    function resolved (s : my_utype_vector) return my_utype;

    subtype my_type is resolved my_utype;
    subtype my_type_vector is (resolved) my_utype_vector;
    subtype my_type_vector_vector is ((resolved)) my_utype_vector_vector;

    function resolved (s : my_utype_vector) return my_utype is
        variable result : my_utype := s(s'low);
    begin
        for i in s'range loop
            if my_utype'pos(s(i)) > my_utype'pos(result) then
                result := s(i);
            end if;
        end loop;
        return result;
    end function;

    signal x : my_type;
    signal y : my_type_vector(1 to 3);
    signal z : my_type_vector_vector(1 to 3);
begin

    x <= b;
    y <= (a, b, c);
    z <= (others => (a, b, c));

    process is
    begin
        x <= a;
        wait for 1 ns;
        assert x = b;
        x <= c;
        wait for 1 ns;
        assert x = c;
        y <= (b, b, b);
        wait for 1 ns;
        assert y = (b, b, c);
        z <= ((a, a, a), (b, b, b), (c, c, c));
        wait for 1 ns;
        assert z = ((a, b, c), (b, b, c), (c, c, c));
        wait for 1 ns;
        wait;
    end process;

end architecture;
