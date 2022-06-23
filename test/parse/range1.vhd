entity range1 is
end entity;

architecture test of range1 is
begin

    p1: process is
        variable x : character;
    begin
        for i in 1 to 3 loop            -- OK
        end loop;
        for i in character'range loop   -- OK
        end loop;
        for i in character loop         -- OK
        end loop;
        for i in x'range loop           -- Error
        end loop;
        for i in x loop                 -- Error
        end loop;
        for i in 4 loop                 -- Error
        end loop;
        for i in integer range 1 to 3 loop   -- OK
        end loop;
        for i in x range 'a' to 'b' loop  -- Error
        end loop;
        wait;
    end process;

end architecture;
