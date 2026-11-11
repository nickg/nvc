entity procs1_sub is
    port ( x : in integer;
           y : out integer );
end entity;

architecture test of procs1_sub is
begin

    follow: process (x) is
    begin
        y <= x + 1;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity procs1 is
end entity;

architecture test of procs1 is
    signal a, b : integer := 0;
begin

    driver: process is
    begin
        wait for 10 ns;
        a <= 1;
        wait for 10 ns;
        a <= 2;
        wait;
    end process;

    uut: entity work.procs1_sub
        port map ( a, b );

end architecture;
