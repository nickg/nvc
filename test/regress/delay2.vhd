entity delay2 is
end entity;

architecture test of delay2 is
    signal clk : bit;
begin

    clock_p: process is
    begin
        if now < 1 us then
            clk <= '1' after 5 ns, '0' after 10 ns;
            wait for 10 ns;
        else
            wait;
        end if;
    end process;

    check_p: process is
        variable now_ns : integer;
    begin
        if now < 1 us then
            wait for 0 ns;
            now_ns := integer(now / ns);
            case now_ns mod 10 is
                when 0 =>
                    assert clk = '0';
                when 5 =>
                    assert clk = '1';
                when others =>
                    report "clk changed at unexpected time";
            end case;
            wait for 5 ns;
        else
            wait;
        end if;
    end process;

end architecture;
