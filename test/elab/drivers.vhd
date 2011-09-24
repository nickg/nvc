entity drivers is
end entity;

architecture a of drivers is
    signal x, y : integer;
begin

    first: process is
    begin
        x <= 5;
        x <= 2;
        y <= 7;
    end process;

    second: process is
    begin
        y <= 7;
        y <= 24;                        -- Error: unresolved signal with
                                        -- multiple drivers
    end process;
    
end architecture;
