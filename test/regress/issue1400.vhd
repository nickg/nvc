entity sub is
end entity;

architecture tb of sub is
    signal clk : bit := '0';
    signal a : integer;
begin

    -- psl default clock is clk'event;

    -- psl txtb_lock_only_in_rdy_asrt : assert never
    --  (a = 1)
    --  report "Should not happend";

end architecture;

entity issue1400 is
end entity;

architecture tb of issue1400 is
begin

    -- Commenting out the generate and instantiating only the "sub"
    -- gets rid of the problem!
    my_gen: for i in 0 to 2 generate
        i_sub : entity work.sub;
    end generate;

end architecture;
