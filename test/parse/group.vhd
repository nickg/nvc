entity test is
end entity;

architecture test of test is

    group local_ports is (signal <>);   -- OK

    signal s : bit;

    group local_sigs : local_ports ( s );  -- OK

begin

    process is
        group some_var is (variable);   -- OK
        variable v : integer;
        group g : some_var (v);         -- OK
    begin
    end process;

end architecture;
