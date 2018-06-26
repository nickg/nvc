entity stack1 is
end entity;

architecture arch of stack1 is
    signal clk : bit;
begin
    process
        variable cnt : natural;
    begin
        if(clk='0') then
            clk <= '1';
        else
            clk <= '0';
        end if;
        if now < 50 ns then
            wait for 2 ns;
        end if;
        cnt := cnt + 1;
        if cnt = 1000000 then
            wait;
        end if;
    end process;

end arch;
