entity loop1 is
end entity;

architecture test of loop1 is
begin

    p1: process is
        variable a, b : integer;
    begin
        loop
            exit when a = 10;
            a := a + 1;
        end loop;
        loop
            a := a + 1;
            next when (a mod 2) = 0;
            b := b + 1;
            exit when b = 10;
        end loop;
        wait;
    end process;

end architecture;
