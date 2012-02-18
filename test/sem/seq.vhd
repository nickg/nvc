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
    
    -- For
    process is
        variable v : integer;
    begin
        for i in 0 to 10 loop           -- OK
            v := i + 1;
        end loop;
        for i in bit'range loop         -- OK
            null;
        end loop;
        for i in x'range loop           -- Error
            null;
        end loop;
    end process;

    -- Case
    process is
        type letter is (A, B, C);
        variable l : letter;
    begin
        l := A;
        case l is                       -- OK
            when a =>
                null;
            when b | c =>
                null;
        end case;
        case l is                       -- OK
            when a =>
                null;
            when others =>
                null;
        end case;
        case l is                       -- Others not last
            when others =>
                null;
            when a =>
                null;
        end case;
        case l is
            when l =>                   -- Not locally static
                null;
            when others =>
        end case;
    end process;

    -- Exit
    process is
    begin
        loop
            exit when false;            -- OK
            exit when 1;                -- Not boolean
        end loop;
    end process;

    -- Procedure call
    process is
        procedure add1(x : in integer; y : out integer) is
        begin
            y := x + 1;
        end procedure;

        variable a, b : integer;
    begin
        add1(a, b);                     -- OK
        add1(1, b);                     -- OK
        add1(3, 6);                     -- Error
    end process;
    
end architecture;
