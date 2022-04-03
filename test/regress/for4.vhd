entity for4 is
end entity;

architecture test of for4 is
begin

    main: process is
        variable n, count : natural;
    begin
        n := 6;
        for i in 1 to n loop            -- Discrete range evaluated here
            n := 3;                     -- Does not affect loop trip count
            count := count + 1;
        end loop;
        assert count = 6;
        wait;
    end process;

end architecture;
