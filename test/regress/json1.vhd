entity json1_sub is
    port ( clk : in bit;
           q   : out integer );
end entity;

architecture test of json1_sub is
begin

    count: process (clk) is
        variable n : integer := 0;
    begin
        if clk = '1' then
            n := n + 1;
            q <= n;
        end if;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity json1 is
end entity;

architecture test of json1 is
    signal clk : bit;
    signal cnt : integer;
    signal s   : string(1 to 3) := "a""b";
begin

    clkgen: clk <= not clk after 5 ns when now < 40 ns;

    uut: entity work.json1_sub
        port map ( clk, cnt );

end architecture;
