entity issue1616 is
end entity;

architecture test of issue1616 is
begin
    process is
    begin
        wait for 10 ns;
        report "VHPI callback did not end simulation" severity failure;
    end process;
end architecture;
