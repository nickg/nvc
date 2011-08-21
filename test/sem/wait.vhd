entity a is
end entity;

architecture b of a is
begin

    -- wait for
    process is
    begin
        wait for ps;
        wait for 5 ns;
        wait for 2 * 4 hr;
        wait for 523;                -- Not TIME type
    end process;    

end architecture;
    
