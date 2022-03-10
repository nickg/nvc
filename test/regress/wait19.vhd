entity wait19 is
end entity;

architecture test of wait19 is
    signal x : bit;
begin

    main: process is
    begin
        x <= not x;
        wait for 100 ms;                -- Should terminate once time
                                        -- reaches TIME'HIGH
    end process;

end architecture;
