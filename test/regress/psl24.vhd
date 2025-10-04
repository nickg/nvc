entity psl24 is
end entity;

architecture tb of psl24 is

    signal clk : natural;

    signal a : bit := '1';

begin

    clkgen: clk <= clk + 1 after 1 ns when clk < 5;

    -- psl default clock is clk'delayed(0 ns)'event;

    -- psl assert never (a = '1') report "Custom message";

end architecture;
