-- tracing_on foo bar
architecture test of synth is
    signal x : integer;
    signal y : bit;
begin

    process is
    begin
        -- lint_on x y z
        x <= 5;
    end process;

    -- lint_off

end architecture;
