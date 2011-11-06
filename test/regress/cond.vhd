entity cond is
end entity;

architecture test of cond is
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
            report "x /= y + 1";
        end if;
        wait;
    end process;
    
end architecture;
