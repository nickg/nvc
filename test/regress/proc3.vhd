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

begin

    process is
    begin
        p1;
        assert now = 15 ns;
        p2;
        assert now = 45 ns;
        p3(5 ns);
        assert now = 100 ns;
        wait;
    end process;

end architecture;
