-- Generic overrides of a subtype of a physical type
entity genphys1 is
    generic ( D : delay_length := 0 ns );
end entity;

architecture test of genphys1 is
begin
    check: process is
    begin
        assert D = 3 ns report time'image(D);
        wait;
    end process;
end architecture;
