entity while1 is
end entity;

architecture test of while1 is
begin

    p1: process is
        variable n : integer := 5;
    begin
        while n > 0 loop
            n := n - 1;
        end loop;
        wait;
    end process;

end architecture;
