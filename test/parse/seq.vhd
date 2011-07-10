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
    end process;
    
end architecture;
