entity while1 is
end entity;

architecture test of while1 is
begin

    process is
        variable n : integer := 5;
    begin
        while n > 0 loop
            report integer'image(n);
            n := n - 1;
        end loop;
        while n < 5 loop
            report integer'image(n);
            n := n + 1;
            wait for 1 ns;
        end loop;
        wait;
    end process;
    
end architecture;
