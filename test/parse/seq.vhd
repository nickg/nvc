architecture a of b is
begin

    -- Wait statements
    process is
    begin
        wait for 1 ns;
        block_forever: wait;
        wait on x;
        wait on x, y, z(1 downto 0);
        wait on w(1) for 2 ns;
        wait until x = 3;
        wait until y = x for 5 ns;
        wait on x until x = 2 for 1 ns;
    end process;

    -- Blocking assignment
    process is
        variable a : integer;
    begin
        a := 2;
        a := a + (a * 3);
    end process;

    -- Assert and report
    process is
    begin
        assert true;
        assert false severity note;
        assert 1 > 2 report "oh no" severity failure;
        report "hello";
        report "boo" severity error;
    end process;

    -- Function calls
    process is
    begin
        x := foo(1, 2, 3);
        a := "abs"(b);
    end process;

    -- If
    process is
    begin
        if true then
            x := 1;
        end if;
        test: if true then
            x := y;
        end if test;
        if x > 2 then
            x := 5;
        else
            y := 2;
        end if;
        if x > 3 then
            null;
        elsif x > 5 then
            null;
        elsif true then
            null;
        else
            x := 2;
        end if;
    end process;

    -- Null
    process is
    begin
        null;
    end process;

    -- Return
    process is
    begin
        return 4 * 4;
    end process;

    -- While
    process is
    begin
        while n > 0 loop
            n := n - 1;
        end loop;
        loop
            null;
        end loop;
    end process;

    -- Delayed assignment
    process is
    begin
        x <= 4 after 5 ns;
        x <= 5 after 1 ns, 7 after 8 ns;
        x <= 5, 7 after 8 ns;
        x <= inertial 5;
        x <= transport 4 after 2 ns;
        x <= reject 4 ns inertial 6 after 10 ns;
    end process;

    -- For
    process is
    begin
        for i in 0 to 10 loop
            null;
        end loop;
        for i in foo'range loop
            null;
        end loop;
    end process;

    -- Exit
    process is
    begin
        exit;
        exit when x = 1;
    end process;

    -- Procedure call
    process is
    begin
        foo(x, y, 1);
        bar;
        foo(a => 1, b => 2, 3);
    end process;

    -- Case
    process is
    begin
        case x is
            when 1 =>
                null;
            when 2 =>
                null;
            when 3 | 4 =>
                null;
            when others =>
                null;
        end case;
    end process;

    -- Next
    process is
    begin
        next;
        next when foo = 5;
    end process;

    -- Signal assignment to aggregate
    process is
    begin
        ( x, y, z ) <= foo;
    end process;

end architecture;
