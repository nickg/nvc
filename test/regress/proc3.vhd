entity proc3 is
end entity;

architecture test of proc3 is

    procedure p1 is
    begin
        wait for 10 ns;
        wait for 5 ns;
    end procedure;

    procedure p2 is
    begin
        p1;
        p1;
    end procedure;

    procedure p3(t : in time) is
    begin
        loop
            wait for t;
            if now >= 100 ns then
                return;
            end if;
        end loop;
    end procedure;

    procedure p4(x : in integer; y : out integer) is
        variable k : integer;
    begin
        k := x;
        for i in 1 to 5 loop
            k := k + 1;
            wait for 1 ns;
        end loop;
        y := k;
    end procedure;

begin

    process is
        variable x : integer;
    begin
        p1;
        assert now = 15 ns;
        p2;
        assert now = 45 ns;
        p3(5 ns);
        assert now = 100 ns;
        p4(5, x);
        assert x = 10;
        assert now = 105 ns;
        wait;
    end process;

end architecture;
