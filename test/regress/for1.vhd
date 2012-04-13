entity for1 is
end entity;

architecture test of for1 is
begin

    process is
        variable x : integer;
    begin
        for i in 1 to 5 loop
            report integer'image(i);
        end loop;
        for i in 100 downto 95 loop
            report integer'image(i);
        end loop;
        x := -1;
        for i in 1 to x loop
            null;
        end loop;
        x := 4;
        for i in 5 to x loop
            null;
        end loop;
        wait;
    end process;
    
end architecture;
    
