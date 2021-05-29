entity synth is end entity;

architecture test of synth is
    -- synthesis translate_off
    signal x : integer;                     -- Should ignore
    -- synthesis translate_on
    signal y : bit;
begin

    process is
    begin
        -- synthesis translate_off    should ignore this trailing comment
        x <= 5;
    end process;

    process is
    begin
        -- synthesis translate_on
        -- Should only get one process here
        y <= '1';
    end process;

end architecture;
