entity names4 is
end entity;

architecture test of names4 is
    type pt is protected
        function fn(x, y : integer) return integer;
        function fn(x, y : real) return real;
    end protected;

    type pt is protected body
        function fn(x, y : integer) return integer is
        begin
            return x + y;
        end function;
        function fn(x, y : real) return real is
        begin
            return x + y;
        end function;
    end protected body;

    shared variable sv : pt;
begin

    process is
        variable v : integer;
    begin
        -- Converted from T_ARRAY_REF
        v := sv.fn(1, y => 1);
        v := sv.fn(1, y => 1) + 0;
    end process;

end architecture;
