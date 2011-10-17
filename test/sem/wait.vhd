entity a is
end entity;

architecture b of a is
    signal x, y : bit;
begin

    -- wait for
    process is
    begin
        wait for ps;
        wait for 5 ns;
        wait for 2 * 4 hr;
        wait for 523;                -- Not TIME type
    end process;

    -- wait on
    process is
        variable v : bit;
    begin
        wait on x;
        wait on x, y;
        wait on v;                      -- Not signal
    end process;

end architecture;
    
