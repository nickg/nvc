entity counter_bot is
    port (
        clk   : in bit;
        count : out integer );
end entity;

architecture behav of counter_bot is
begin

    process (clk) is
        variable count_var : integer := 0;
    begin
        if clk'event and clk = '1' then
            count_var := count_var + 1;
            count <= count_var;
        end if;
    end process;

end architecture;

-------------------------------------------------------------------------------
    
entity counter is
end entity;

architecture test of counter is
    signal clk   : bit := '0';
    signal count : integer := 0;
begin

    clkgen: process is
    begin
        wait for 5 ns;
        clk <= not clk;
    end process;

    uut: entity work.counter_bot
        port map (
            clk   => clk,
            count => count );

    process (count) is
    begin
        report integer'image(count);
    end process;
    
end architecture;
