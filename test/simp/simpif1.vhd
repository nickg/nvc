entity simpif1 is
end entity;

architecture test of simpif1 is
    signal s : integer;
    constant c : integer := 5;
begin

    p1: process is
        variable v : integer := 0;
    begin
        if c > 2 then
            if c < 10 then
                v := 2;
                wait for 1 ns;
            end if;
            wait for 1 ns;
        end if;
        wait;
    end process;

    p2: process is
        variable v : integer := 0;
    begin
        if c > v then
            v := 2;
        elsif c = 5 then
            v := 7;
        else
            s <= 1;
        end if;
    end process;

    p3: process is
        variable v : integer := 0;
    begin
        if c = 2 then
            v := 2;
        elsif c = v then
            s <= 1;
        else
            v := 7;
        end if;
    end process;

    p4: process is
    begin
        if false then
            s <= 5;
        end if;
    end process;

end architecture;
