entity issue1427 is
end entity ;

architecture arch of issue1427 is

    signal clock : bit ;
    signal data  : bit_vector(7 downto 0) ;

    -- User-defined attributes on the underlying signals
    attribute hw_name : string ;
    attribute hw_name of clock : signal is "pclk100" ;
    attribute hw_name of data  : signal is "data_bus" ;

    -- One-level aliases
    alias aclock is clock ;
    alias adata  is data ;

    -- Two-level alias chains (alias of an alias)
    alias aclock2 is aclock ;
    alias adata2  is adata ;

begin

    tb : process
    begin

        assert clock'hw_name  = "pclk100" report "FAIL: clock'hw_name" severity failure ;
        assert aclock'hw_name = "pclk100" report "FAIL: aclock'hw_name should equal clock'hw_name" severity failure ;

        assert data'hw_name  = "data_bus" report "FAIL: data'hw_name" severity failure ;
        assert adata'hw_name = "data_bus" report "FAIL: adata'hw_name should equal data'hw_name" severity failure ;

        assert aclock2'hw_name = "pclk100"  report "FAIL: aclock2'hw_name should equal clock'hw_name" severity failure ;
        assert adata2'hw_name  = "data_bus" report "FAIL: adata2'hw_name should equal data'hw_name" severity failure ;

        assert clock'simple_name   = "clock"   report "FAIL: clock'simple_name" severity failure ;
        assert aclock'simple_name  = "aclock"  report "FAIL: aclock'simple_name should be aclock, not clock" severity failure ;
        assert aclock2'simple_name = "aclock2" report "FAIL: aclock2'simple_name should be aclock2, not aclock or clock" severity failure ;

        assert clock'instance_name   = ":issue1427(arch):clock"   report "FAIL: clock'instance_name" severity failure ;
        assert aclock'instance_name  = ":issue1427(arch):aclock"  report "FAIL: aclock'instance_name should be :test(arch):aclock" severity failure ;
        assert aclock2'instance_name = ":issue1427(arch):aclock2" report "FAIL: aclock2'instance_name should be :test(arch):aclock2" severity failure ;

        assert clock'path_name   = ":issue1427:clock"   report "FAIL: clock'path_name" severity failure ;
        assert aclock'path_name  = ":issue1427:aclock"  report "FAIL: aclock'path_name should be :test:aclock" severity failure ;
        assert aclock2'path_name = ":issue1427:aclock2" report "FAIL: aclock2'path_name should be :test:aclock2" severity failure ;

        report "All checks passed" ;
        std.env.stop ;
    end process ;

end architecture ;
