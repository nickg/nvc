entity a is
end entity;

architecture b of a is
    signal i : integer := 5;
    signal t : time := 5 fs;
    signal c : character := 'x';

    subtype my_time is time;

    constant x : my_time := my_time'left + 5 ns;
begin

    process is
    begin
        wait for 4 ns;
        t <= now;
    end process;

    process is
        -- Default values
        variable a : time;
        variable b : my_time;
    begin
    end process;

    process is
        variable t : time := -1 ns;
        variable u : time := 1.2 ns;
    begin
    end process;

end architecture;
