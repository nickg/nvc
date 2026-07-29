entity stage is
    generic (
        COUNT : natural
    );
end entity;

architecture bhv of stage is
begin
    GenReg : for c in 0 to COUNT-1 generate
        signal reg : integer := -1;
    begin
        ProcReg : process is
            procedure init is
            begin
                reg <= 42 + c;
            end procedure;
        begin
            init;
            wait for 0 ns;
            assert reg = 42 + c;
            wait;
        end process;
    end generate;
end architecture;

-------------------------------------------------------------------------------

entity issue1627 is
end entity;

architecture test of issue1627 is
begin
    InstA : entity work.stage
        generic map (
            COUNT => 1
        );

    InstB : entity work.stage
        generic map (
            COUNT => 0
        );

    InstC : entity work.stage
        generic map (
            COUNT => 1
        );
end architecture;
