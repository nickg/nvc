entity driver1 is
end entity;

architecture test of driver1 is
    signal s : bit_vector(1 to 3);
begin

    p1: process is
    begin
        if 3 < 2 then                   -- Cannot delete this
            s(5) <= '1';                -- Error
        else
            s(7) <= '0';                -- Error
        end if;
        while false loop
            s(-1) <= '0';               -- Error
        end loop;
        wait;
    end process;

end architecture;
