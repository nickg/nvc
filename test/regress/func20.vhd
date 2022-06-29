entity func20 is
end entity;

architecture test of func20 is

    impure function outer return string is
        variable s : string(1 to 5);

        impure function inner return string is
        begin
            return s;
        end function;
    begin
        s := "hello";
        return inner;
    end function;

begin

    p1: process is
    begin
        assert outer = "hello";
        wait;
    end process;

end architecture;
