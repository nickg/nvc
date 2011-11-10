entity cond1 is
end entity;

architecture test of cond1 is
    signal x : integer := 5;
begin

    process is
        variable y : integer;
    begin
        y := 5;
        if x = y then
            report "x = y";
            x <= 4;
        end if;
        if x = y + 1 then
            report "x = y + 1" severity failure;
        else
            null;
            report "x /= y + 1";
        end if;
        if x = y - 1 then
            report "x = y - 1" severity failure;
        elsif x = y then
            report "x = y still";
        else
            report "x /= y - 1 and x /= y" severity failure;
        end if;
        wait;
    end process;
    
end architecture;
