package generic_pkg is
    generic (type              g_countertype ;
	     g_initval :       g_countertype ;
	     function "+" (l : g_countertype ; r : integer) return g_countertype );

    subtype countertype is g_countertype;
    constant initval : g_countertype := g_initval;

end package;

--------------------------------------------------

entity gentest is
    generic ( package test_pkg is new work.generic_pkg generic map(<>) );
    port ( i : in test_pkg.countertype;
           o : out test_pkg.countertype := test_pkg.initval );
end entity gentest;

architecture test of gentest is
    use test_pkg.all;
begin
    o <= i + 1;
end test;

--------------------------------------------------

entity issue925 is
end;

architecture test of issue925 is

    package test_pkg is new work.generic_pkg
	generic map (g_countertype => integer, g_initval => 0, "+" => "+");
    use test_pkg.all;

    signal x, y : countertype := initval;

begin
    u_gentest : entity work.gentest
	generic map (test_pkg)
	port map (x, y);

    check: process is
    begin
	for i in 1 to 10 loop
	    x <= i;
	    wait for 1 ns;
	    assert y = i + 1;
	end loop;
	wait;
    end process;
end architecture;
