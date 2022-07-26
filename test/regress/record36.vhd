entity record36 is
end entity;

architecture test of record36 is

    type rec is record
        s : string;
    end record;

    procedure test (r : inout rec) is
        variable rr : r'subtype;
    begin
        rr := r;
        for i in rr.s'range loop
            rr.s(i) := 'X';
        end loop;
        r := rr;
    end procedure;

begin

    p1: process is
        variable r : rec(s(1 to 3));
    begin
        r := (s => "abc");
        test(r);
        assert r.s = "XXX";
        r.s := "foo";
        test(r);
        assert r.s = "XXX";
        wait;
    end process;

end architecture;
