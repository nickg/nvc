entity psl23 is
end entity;

architecture tb of psl23 is

    signal clk : natural;

    signal b : bit := '0';
    signal a : bit := '1';

begin

    process
    begin
        wait for 500 ps;
    	for i in 0 to 10 loop
        	clk <= clk + 1;
        	wait for 1 ns;
        end loop;
        wait;
    end process;

    process
    begin
        wait for 3 ns;
        b <= '1';
        wait for 4 ns;
        b <= '0';
        wait;
    end process;

    -- psl default clock is clk'event;

    -- Shall fail at 9.5 ns and 10.5 ns
    -- psl one: assert always (a -> next_e[2 to 4] (b='1')) report "one";

    -- Shall fail at 2.5 ns, 9.5 ns and 10.5 ns
    -- psl two: assert always (a -> next_e[0 to 2] (b='1')) report "two";

    -- Shall fail at 9.5 ns, 10.5 ns and at the end of simulation for
    -- non-finished assertions.
    -- psl three: assert always (a -> next_e![2 to 4] (b='1')) report "three";

    -- Shall
    -- psl four: assert always (a -> next_a[7 to 9] (b='1')) report "four";

end architecture;
