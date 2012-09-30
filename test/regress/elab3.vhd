entity sub is
end entity;

architecture test of sub is
    signal p : integer;
begin

    process is
    begin
        wait for 2 ns;
        report p'instance_name;
        report p'path_name;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity elab3 is
end entity;

architecture test of elab3 is
    signal x : integer;
begin

    s: entity work.sub;

    b: block is
        signal y : integer;
    begin

        process is
        begin
            wait for 1 ns;
            report y'instance_name;
            report y'path_name;
            wait;
        end process;
        
    end block;

    process is
    begin
        report x'instance_name;
        report x'path_name;
        wait;
    end process;

end architecture;

