entity issue676 is
end entity;

architecture test of issue676 is
    signal x, y : natural;
begin

    p1: process (all) is
        variable tmp1, tmp2 : natural;
    begin
        loop
            exit when tmp1 > x;
            tmp1 := tmp1 + 1;
            next when tmp1 rem 2 = 0;
            tmp2 := tmp2 + 1;
            if tmp1 > 100 then
                exit;
            end if;
        end loop;
        y <= tmp2;
    end process;

    check: process is
    begin
        x <= 2;
        wait for 0 ns;
        assert y = 1;
        wait for 0 ns;
        assert y = 2;
        x <= 10;
        wait for 1 ns;
        assert y = 6;
        x <= 1000;
        wait for 1 ns;
        assert y = 51;

        wait;
    end process;

end architecture;
