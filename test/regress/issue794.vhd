entity ent is
end entity;

architecture test of ent is
    signal c : integer := 42;
begin

    process is
    begin
        assert << signal .ent.c : integer >> = 42;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

configuration issue794 of ent is
    for test
    end for;
end configuration;
