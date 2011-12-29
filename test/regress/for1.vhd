entity for1 is
end entity;

architecture test of for1 is
begin

    process is
    begin
        for i in 1 to 5 loop
            report integer'image(i);
        end loop;
        for i in 100 downto 95 loop
            report integer'image(i);
        end loop;
        wait;
    end process;
    
end architecture;
    
