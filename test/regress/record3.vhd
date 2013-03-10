entity record3 is
end entity;

architecture test of record3 is

    procedure add1(x : inout integer) is
    begin
        x := x + 1;
    end procedure;

    type rec is record
        a, b : integer;
    end record;

    procedure foo(r : inout rec) is
    begin
        add1(r.a);
        add1(r.b);
    end procedure;

begin

    process is
        variable r : rec;
    begin
        r := (1, 2);
        foo(r);
        assert r.a = 2;
        wait;
    end process;

end architecture;
