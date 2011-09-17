architecture a of b is
begin

    -- Wait statements
    process is
    begin
        wait for 1 ns;
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
    
end architecture;
