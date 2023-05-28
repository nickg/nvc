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
        variable v : bit_vector(0 to 3);
        constant k : bit := '1';
        variable n : bit;
        variable i : integer;
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
        case v is
            when "0101" =>              -- OK
                null;
            when "1101" =>              -- OK
                null;
        end case;
        case v is
            when (0 to 3 => k) =>       -- OK
                null;
            when (0 to 3 => n) =>       -- Not locally static
                null;
        end case;
        case i is
            when 1 =>                   -- OK
                null;
            when integer'(5) =>         -- OK
                null;
            when (1 + 5) * 7 =>         -- OK
                null;
            when i + 2 =>               -- Not locally static
                null;
        end case;
        case bit is                     -- Error
            when '1' => null;
            when '0' => null;
        end case;
        case i is
            when 1 to 6 =>              -- OK
                null;
            when 7 to 6.1612 =>         -- Error
                null;
        end case;
        case i is
            when n to k =>              -- Error
                null;
        end case;
        case i is
            when 1 to i =>              -- Error
                null;
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

    -- Next
    process is
    begin
        loop
            next when false;            -- OK
            next when 1;                -- Not boolean
            next;                       -- OK
        end loop;
        next;                           -- Not in loop
        l1: loop
            next foo;                   -- Not a label
        end loop;
        l2: loop
            l3: loop
                l4: loop
                    next l2;            -- OK
                end loop;
            end loop;
        end loop;
    end process;

    -- Statement labels
    dup: process is begin end process;
    dup: process is begin end process;

    -- Loop over enumeration
    process is
    begin
        for c in character loop         -- OK
        end loop;
        for c in integer loop           -- OK
        end loop;
        for c in real loop              -- Error
        end loop;
    end process;

    -- Signal assignment to variable
    process is
        variable i : integer;
    begin
        i <= 1;                         -- Error
    end process;

    -- Corner cases in discrete range
    drange: process is
    begin
        for i in 1 range 1 to 2 loop    -- Error
        end loop;
        for i in integer range 1.0 to 2.0 loop  -- Error
        end loop;
        for i in real range real'(1.0) to real'(2.0) loop  -- Error
        end loop;
    end process;

    -- Variable assignment to aggregate
    process is
        type int_vec is array (natural range <>) of integer;
        variable v : int_vec(1 to 2);
        variable a, b : integer;
    begin
        (a, b) := v;                    -- OK
        (a, 1) := v;                    -- Error
        (v(a), b) := v;                 -- Error
        (others => a) := v;             -- Error
        (1 to 2 => a) := v;             -- Error
    end process;

    -- Wrong class for procedure arguments
    process is
        procedure p1(signal x : bit);
        variable v : bit;
    begin
        p1(v);                          -- Error
        p1('1');                        -- Error
    end process;

    -- Invalid use of loop parameter
    process is
        procedure p1 (x : out integer);
    begin
        for i in 1 to 10 loop
            i := 5;                     -- Error
            p1(i);                      -- Error
        end loop;
    end process;

    -- Case statement with error in type
    process is
        variable x : bad_type;          -- Error
    begin
        case x is                       -- Error (suppressed)
            when others => null;
        end case;
    end process;

end architecture;
