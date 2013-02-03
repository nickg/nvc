entity loop1 is
end entity;

architecture test of loop1 is
begin

    process is
        variable a, b, c : integer;
    begin
        a := 0;
        loop
            exit when a = 10;
            a := a + 1;
        end loop;
        assert a = 10;

        a := 0;
        b := 0;
        loop
            a := a + 1;
            next when (a mod 2) = 0;
            b := b + 1;
            exit when b = 10;
        end loop;
        assert b = 10;
        assert a = 19;

        a := 0;
        b := 0;
        loop
            a := a + 1;
            if (a mod 2) = 0 then
                next;
            end if;
            b := b + 1;
            exit when b = 10;
        end loop;
        assert b = 10;
        assert a = 19;

        a := 0;
        outer: loop
            for i in 1 to 10 loop
                a := a + 1;
                exit outer when a = 50;
            end loop;
        end loop;

        wait;
    end process;

end architecture;
