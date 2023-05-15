entity lcs2016_72b is
end entity;

architecture test of lcs2016_72b is

    function iota return result_t of integer_vector is  -- OK
        variable result : result_t;
        variable counter : natural;
    begin
        for i in result'range loop
            result(i) := counter;
            counter := counter + 1;
        end loop;
        return result;
    end function;

    function resize(x : integer_vector) return rv_t of integer_vector is
        alias xa : integer_vector(x'length - 1 downto 0) is x ;
        variable rv : rv_t  := (others => 0);
        alias ra : integer_vector(rv'length - 1 downto 0) is rv ;
        constant bound : integer := minimum(x'length, rv'length) - 2 ;
    begin
        if bound >= 0 then
            rv(bound downto 0) := xa(bound downto 0);
        end if;
        return rv ;
    end function ;

    signal s1 : integer_vector(1 to 2) := iota;  -- OK

begin

    process is
        variable v1 : integer_vector(1 to 3);
        variable v2 : integer;
        constant c1 : integer_vector(1 to 2) := iota;  -- OK
        constant c2 : integer_vector := iota;  -- Error
    begin
        v1 := iota;                     -- OK
        v2 := iota;                     -- Error
        assert iota = (1, 2, 3);        -- Error
        (v1, v2) := iota;               -- Error
        wait;
    end process;

    process is
        function return_int return rv_t of integer is  -- Error
            variable r : rv_t;
        begin
            return r;
        end function;

        subtype bv3_t is bit_vector(1 to 3);

        function return_carray return rv_t of bv3_t is  -- Error
            variable r : rv_t;
        begin
            return r;
        end function;
    begin
    end process;

end architecture;
