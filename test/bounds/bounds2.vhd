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

        b6 <= true after -10 ns when true else false;
        b7 <= true when true else false after -10 ns;

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

end architecture;
