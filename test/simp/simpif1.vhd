entity simpif1 is
end entity;

architecture test of simpif1 is
    signal s : integer;
    constant c : integer := 5;
begin

    p1: process is
    begin
        if c > 2 then
            if c < 10 then
                s <= 2;
                wait for 1 ns;
            end if;
            wait for 1 ns;
        end if;
        wait;
    end process;

end architecture;
