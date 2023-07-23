entity sub is
    port ( i : integer );
end entity;

architecture test of sub is
begin

    process (i) is
    begin
        if i > 0 then                   -- Taken in U1
            report "> 0";
        else                            -- Taken in U1 and U2
            report "< 0";
        end if;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity cover17 is
end entity;

architecture test of cover17 is
    signal x, y : integer;
begin

    u1: entity work.sub port map ( x );
    u2: entity work.sub port map ( y );

    tb: process is
    begin
        x <= 0;
        wait for 1 ns;
        if x = 0 then
            y <= 600;
        end if;
        wait for 1 ns;
        if x < 0 then                   -- Never taken
            assert false;               -- Not covered
        end if;
        wait;
    end process;

end architecture;
