entity bounds2 is
end entity;

architecture test of bounds2 is
begin

    asssignment_delays: block
        signal b1,b2,b3,b4,b5,b6,b7 : boolean;
    begin
        b1 <= true;                     -- OK
        b2 <= true after 10 ns;         -- OK
        b3 <= true after 0 ns;          -- OK
        b4 <= true after -1 ns;         -- Error

        process
        begin
            b5 <= true;                 -- OK
            b5 <= true after 0 ns;      -- OK
            b5 <= true after 1 fs;      -- OK
            b5 <= true after -1 fs;     -- Error
            wait;
        end process;

        b6 <= true after -10 ns when now = 5 ns else false;
        b7 <= true when now = 1 ns else false after -10 ns;

    end block;

    rejection_limits: block
        signal b1,b2,b3 : boolean;
    begin
        b1 <= reject  10 ns inertial true after 10 ns;  -- OK
        b2 <= reject -10 ns inertial true;              -- Error
        b3 <= reject  10 ns inertial true after 5 ns;   -- Error
    end block;

    process
    begin
        wait for -10 ns;            -- Error
        wait;
    end process;

    default_values: block
        type r is range 0 to 1;

        constant        ok1 : integer   range 0     to 1     := 1;      -- OK
        constant        ok2 : character range 'a'   to 'z'   := 'b';    -- OK
        constant        ok3 : real      range 0.0   to 1.0   := 0.0;    -- OK
        constant        ok4 : time      range 10 ns to 20 ns := 10 ns;  -- OK
        constant        ok5 : r                              := 0;      -- OK

        signal          s   : integer   range 0     to 9     := 20;     -- Error
        constant        c1  : character range 'a'   to 'z'   := 'Z';    -- Error
        shared variable v   : real      range 0.0   to 5.0   := 10.0;   -- Error
        constant        t   : time      range 10 ns to 10 us := 0 fs;   -- Error
        constant        c2  : r                              := 10;     -- Error

        subtype subint is integer range 1 to 10;
        procedure test(a : subint := 30) is
        begin
        end procedure;

        function test(a : character range 'a' to 'b' := 'c') return integer is
        begin
            return 1;
        end function;

        component comp is
            generic (
            g2 : integer range 10 downto 0 := 20
            );
            port (
            p2 : in integer range 0 to 1 := 2
            );
        end component;

    begin
        process is
            variable v2  : real range 0.0 to 5.0 := 5.1;   -- Error
        begin
        end process;
    end block;

    ascending_time: block
        signal s   : integer;
        signal del : time;
    begin
        process
        begin
            s <= 0 after 10 ns, 1 after 11 ns;      -- OK
            s <= 0, 1 after 1 ns;                   -- OK
            s <= 10 after del;                      -- OK
            s <= 10 after del, 20 after del + 1 ns; -- OK

            s <= 0, 1;                           -- Error
            s <= 0 after 1 ns, 1;                -- Error
            s <= 0 after 2 ns, 1 after 1 ns;     -- Error
            s <= 0 after 1 ns, 1 after del, 2;   -- Error
            s <= 1 after del, 2;                 -- Error

            wait;
        end process;
    end block;

    textio1: block is
        function unit_string (unit : time) return string is
        begin
            if unit = fs then           -- OK
                return " fs";
            elsif unit = ps then
                return " ps";
            elsif unit = ns then
                return " ns";
            else
                report "invalid unit " & time'image(unit);
            end if;
        end function;
    begin
    end block;

    typeconv: process is
        type bv10 is array (1 to 10) of bit;
        variable a : bv10;
        variable b : bit_vector(1 to 10);
    begin
        a := bv10(b);                   -- OK
        a := bv10(b(1 to 9));           -- Error
        wait;
    end process;

end architecture;
