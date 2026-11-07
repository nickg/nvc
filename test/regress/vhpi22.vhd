entity vhpi22 is
end entity;

architecture test of vhpi22 is
    signal clk : bit := '0';
begin
    clk <= not clk after 1 ns;
end architecture;
