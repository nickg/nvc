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
    end process;

    -- If
    process is
    begin
        if true then
            x := 1;
        end if;
    end process;

    -- Null
    process is
    begin
        null;
    end process;
    
end architecture;
