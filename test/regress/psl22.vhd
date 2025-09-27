entity psl22 is
end entity;

architecture tb of psl22 is

    signal clk : natural := 0;

    signal  a : bit;

begin

    clkgen: clk <= clk + 1 after 1 ns when clk < 10;

    -- psl default clock is clk'delayed(0 ns)'event;

    -- These assertions are covered random number of times deterministic
    -- based on the seed. Checked with seed=1.

    -- This one fires at 3ns
    -- psl one: cover { nondet({1,3 to 7,9}) = 5} report "one";

    -- This one shall fire at 2,5,8,10 ns
    -- psl two: cover { nondet(boolean) } report "two";

    -- Note: Since there is one global randomizer for all PSL assertions,
    --       removing one, affects gold result of others

end architecture;
