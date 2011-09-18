entity e is
end entity;

architecture a of e is
begin

    process is
    begin
        assert false;                   -- OK
        assert 1 > 2 report "false";    -- OK
        assert 4 < 7 report "true" severity failure;  -- OK
        report "boo" severity note;
    end process;

    process is
    begin
        assert 1;                       -- Not BOOLEAN
    end process;

    process is
    begin
        report 53;                      -- Not STRING
    end process;

    process is
    begin
        report "boo" severity 1;        -- Not SEVERITY_LEVEL
    end process;        
    
end architecture;
