entity sub is
    port (
        x : in integer;
        y : out integer );
end entity;

architecture test of sub is
begin

    process is
    begin
        if x'active then                 -- NULL nets array here
            y <= x + 1;
        end if;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity elab23 is
end entity;

architecture test of elab23 is
    signal y : integer;
begin

    sub_i: entity work.sub
        port map ( x => 0, y => y );

    process is
    begin
        wait for 1 ns;
        assert y = integer'low;
        wait;
    end process;

end architecture;
