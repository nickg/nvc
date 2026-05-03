entity issue1517 is
end entity;

architecture test of issue1517 is
begin
    process
    begin
        for i in 0 to 2 loop
            report "FIRST LOOP";
        end loop;
        for i in 0 to 3 loop
            report "SECOND LOOP";
        end loop;
        wait;
    end process;
end architecture;