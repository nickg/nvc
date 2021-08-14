entity sub is
    generic ( N : integer );

    function addN(x : integer) return integer is
    begin
        return x + N;
    end function;
end entity;

architecture test of sub is
    signal x : integer := 4;
begin

    process is
    begin
        assert addN(x) = x + N;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity jcore1 is
    function add2(x : integer) return integer is
    begin
        return x + 2;
    end function;
end entity;

architecture test of jcore1 is
    signal x : integer := 4;
begin

    process is
    begin
        assert add2(x) = 6;
        wait;
    end process;

    sub_i: entity work.sub
        generic map ( 6 );

    sub2_i: entity work.sub
        generic map ( 7 );

end architecture;
