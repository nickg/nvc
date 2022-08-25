entity issue524 is
end entity;

architecture test of issue524 is
    signal s : real := 0.0;
begin

    main: process is
    begin
        for i in 1 to 5 loop
            s <= s + 0.5;
            wait for 1 ns;
        end loop;
        wait;
    end process;

end architecture;
