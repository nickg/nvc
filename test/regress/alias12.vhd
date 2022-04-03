entity alias12 is
end entity;

architecture test of alias12 is

    function get (x : in bit_vector; n : in natural range 1 to 3) return bit is
        alias a : bit_vector(1 to 3) is x;
    begin
        return a(n);
    end function;

begin

    main: process is
        variable v1 : bit_vector(1 to 3);
        variable v2 : bit_vector(4 to 6);
        variable v3 : bit_vector(2 to 3);
    begin
        v1 := "101";
        v2 := "101";
        v3 := "01";
        wait for 1 ns;
        assert get(v1, 1) = '1';
        assert get(v1, 3) = '1';
        assert get(v2, 1) = '1';
        assert get(v2, 2) = '0';
        assert get(v3, 1) = '0';        -- Error
        wait;
    end process;

end architecture;
