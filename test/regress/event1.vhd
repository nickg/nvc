entity event1 is
end entity;

architecture test of event1 is
    signal si : integer := 0;
    event e1 : boolean := si > 0;
    event e2 : integer := si + 1;
    signal wait_on_ok, wait_until_ok, pcall_ok : boolean;

    procedure wait_for_event(event e : boolean) is
    begin
        wait until e;
    end procedure;
begin

    si <= si + 1 after 1 ns when si < 10;

    -- We can use events like variables in expressions in which case they are
    -- evaluated directly
    process is
    begin
        assert not e1;
        assert e2 = 1;
        wait for 1 ns;
        assert e1;
        assert e2 = 2;
        wait;
    end process;

    -- Boolean events can be used in wait until
    process is
        event e : boolean := si = 9;
    begin
        wait until e1;
        assert now = 1 ns;
        wait until e;
        assert now = 9 ns;
        wait_until_ok <= true;
        wait;
    end process;

    -- Events can be passed to subprograms and created from inline expressions
    process is
    begin
        wait_for_event(e1);
        assert now = 1 ns;
        wait_for_event(si = 5);
        assert now = 5 ns;
        pcall_ok <= true;
        wait;
    end process;

    -- Events can be used in wait on, which resumes when any of the signals in
    -- the sensitivity list changes
    process is
    begin
        wait on e1;
        assert now = 1 ns;
        wait on e1;
        assert now = 2 ns;              -- e1 didn't actually change value here
        wait_on_ok <= true;
        wait;
    end process;

    -- Events capture by reference which may or may not be a good idea
    process is
        variable v : integer := 5;
        event e : boolean := v = 5;
    begin
        assert e;
        v := 7;
        assert not e;
        wait;
    end process;

    -- Events can be (ab)used for lazy evaluation
    process is
        event e : integer := 1 / 0;     -- Never executed

        function expensive_to_string (x : integer) return string is
        begin
            report "do not call this" severity failure;
        end function;

        -- The usual implementation of this in OSVVM and elsewhere would
        -- evaluate msg before calling the procedure
        procedure my_assert_equals(x, y : integer; event msg : string) is
        begin
            if x /= y then
                report "assertion failed: " & msg;
            end if;
        end procedure;
    begin
        if si = 100 then
            report to_string(e);
        end if;
        wait for 1 ns;
        my_assert_equals(si, 1, expensive_to_string(si));
        wait;
    end process;

    process is
    begin
        wait for 10 ns;
        assert wait_on_ok;
        assert wait_until_ok;
        assert pcall_ok;
        wait;
    end process;

end architecture;
