entity func11 is
end entity;

architecture test of func11 is

    function foo(x : integer) return integer is
    begin
        return x + 1;
    end function;

    function foo(x : integer) return real is
    begin
        return real(x) + 1.0;
    end function;

begin

    process is
    begin
        assert foo(1) = 2;
        assert foo(1) = 2.0;
        wait;
    end process;

end architecture;
