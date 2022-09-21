entity cover1 is
end entity;

architecture test of cover1 is
    signal s : integer;
begin

    process is
        variable v : integer;
    begin
        v := 1;
        s <= 2;
        wait for 1 ns;
        if s = 2 or s > 10 then
            v := 3;
        else
            v := 2;
        end if;
        while v > 0 loop
            if v mod 2 = 0 then
                v := v - 1;
            else
                v := (v / 2) * 2;
            end if;
        end loop;
        wait;
    end process;

end architecture;
