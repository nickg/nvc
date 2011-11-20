entity signal6 is
end entity;

architecture test of signal6 is
    signal x : integer := 0;
begin

    process is
    begin
        x <= 1, 2 after 3 ns, 4 after 5 ns, 8 after 9 ns;
        wait;
    end process;

    process (x) is
    begin
        report integer'image(x);
    end process;
    
end architecture;
