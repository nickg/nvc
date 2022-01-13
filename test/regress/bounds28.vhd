entity bounds28 is
end entity;

architecture test of bounds28 is
    signal n : integer;
begin

    main: process is
        variable t : delay_length;
    begin
        n <= 1;
        wait for 1 ns;
        t := n * ms;                    -- OK
        n <= -5;
        wait for 1 ns;
        t := n * fs;                    -- Error
        wait;
    end process;

end architecture;
