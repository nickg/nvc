entity cond2 is
end entity;

architecture test of cond2 is
    signal x, y : integer;
begin

    process is
    begin
        x <= 5;
        y <= 2;
        wait for 1 ns;
        if x = 5 then
            if y = 2 then
                report "y = 2";
                if x = 4 then
                    report "x = 4" severity failure;
                else
                    report "x /= 4";
                end if;
            else
                report "y /= 2" severity failure;
            end if;
        else
            report "x /= 5" severity failure;
        end if;
        
        wait;
    end process;
    
end architecture;
