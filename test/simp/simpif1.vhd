entity simpif1 is
end entity;

architecture test of simpif1 is
    signal s : integer;
    constant c : integer := 5;
begin

    p1: process is
        variable v : integer := 0;
    begin
        if c > 2 then
            if c < 10 then
                v := 2;
                wait for 1 ns;
            end if;
            wait for 1 ns;
        end if;
        wait;
    end process;

end architecture;
