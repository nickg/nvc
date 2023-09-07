entity cpcall is
end entity;

architecture test of cpcall is
    procedure test1 (signal a, b : in bit) is
    begin
        assert a = b;
    end procedure;

    procedure test2 (signal a, b : in bit) is
    begin
        if a = b then
            wait for 1 ns;
        end if;
    end procedure;

    signal x, y : bit;
begin

    p0: test1(x, y);
    p1: test2(x, y);

end architecture;
