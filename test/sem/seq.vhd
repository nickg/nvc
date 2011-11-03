entity seq is
end entity;

architecture test of seq is
begin

    -- If statements
    process is
    begin
        if true then                    -- OK
            report "hello";
        end if;

        if 1 then                       -- Not boolean
        end if;

        if false then
            x := 5;                     -- Error in statement
        end if;
    end process;

    -- Null statements
    process is
    begin
        null;
    end process;
    
end architecture;
