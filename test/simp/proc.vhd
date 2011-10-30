entity proc is
end entity;

architecture test of proc is
    signal x, y : integer;
begin

    -- Test rewrite of process sensitivity list
    process (x, y) is
    begin
        report "awake";
    end process;
    
end architecture;
