entity psl21 is
end entity;

architecture tb of psl21 is

    signal clk : natural;

    signal a : bit := '0';
    signal b : bit := '1';
    signal c : bit := '0';
    signal d : bit := '1';

    signal num1 : natural := 1;
    signal num2 : natural := 2;

    signal bv1 : bit_vector(3 downto 0) := "0101";
    signal bv2 : bit_vector(3 downto 0) := "1110";

begin

    clkgen: clk <= clk + 1 after 1 ns when clk < 10;

    -- psl default clock is clk'delayed(0 ns)'event;

    -- Checked with seed=1 to give 1 at:

    -- These assertions are covered random number of times deterministic
    -- based on the seed
    -- psl one: cover {a union b union c union d} report "one";
    -- psl two: cover {(num1 union num2) = 1} report "two";
    -- psl three: cover {countones(bv1 union bv2) = 3} report "three";

    -- This assertion should never be covered
    -- psl four: cover {countones(bv1 union bv2) = 4} report "four";

    -- Note: Since there is one global randomizer for all PSL assertions,
    --       removing one, affects gold result of others

end architecture;
