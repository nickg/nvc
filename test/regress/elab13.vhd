entity recur is
    generic (
        DEPTH : natural );
end entity;

architecture test of recur is
begin

    base_g: if DEPTH = 0 generate

        process is
        begin
            report recur'path_name;
            wait;
        end process;

    end generate;

    recur_g: if DEPTH > 0 generate

        recur1_i: entity work.recur
            generic map (
                DEPTH => DEPTH - 1 );

        recur2_i: entity work.recur
            generic map (
                DEPTH => DEPTH - 1 );

    end generate;

end architecture;

-------------------------------------------------------------------------------

entity elab13 is
end entity;

architecture test of elab13 is
begin

    top_i: entity work.recur
        generic map (
            DEPTH => 3 );

end architecture;
