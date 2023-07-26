entity predef4 is
end entity;

architecture test of predef4 is
    type FREQ is range 0 to integer'high
        units
            Hz;
            kHz = 1000 Hz;
            MHz = 1000 kHz;
            GHz = 1000 MHz;
        end units;

    type T_FREQVEC is array(natural range <>) of FREQ;
begin

    p1: process is
        variable v : t_freqvec(1 to 3);
    begin
        v := ( 1 hz, 10 khz, 3 mhz );
        wait for 1 ns;
        assert minimum(v) = 1 hz;
        assert maximum(v) = 3 mhz;
        wait;
    end process;

end architecture;
