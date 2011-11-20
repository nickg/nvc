entity seq is
end entity;

architecture test of seq is
begin

    -- If statements
    process is
        variable v : integer;
    begin
        if true then                    -- OK
            report "hello";
        end if;

        if 1 then                       -- Not boolean
        end if;

        if false then
            x := 5;                     -- Error in statement
        end if;

        if true or false then
            null;
        else
            v := true;                  -- Error in else part
        end if;

        if false then
            null;
        elsif true then
            null;
        elsif x > 2 then
            g := v;                     -- Error
        else
            v := 1;
        end if;
    end process;

    -- Null statements
    process is
    begin
        null;
    end process;

    -- Return statements
    process is
    begin
        return 1;                       -- Error
    end process;

    -- While statements
    process is
        variable n : integer := 5;
    begin
        while n > 0 loop                -- OK
            n := n - 1;
        end loop;
        loop                            -- OK
            null;
        end loop;
        loop
            return 5;                   -- Error
        end loop;
        while 5 loop                    -- Error
            null;
        end loop;
    end process;
    
end architecture;
