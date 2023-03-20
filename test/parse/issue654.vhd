package frequency is

    type frequency is range 0 to 2e9 units
        Hz ;
        kHz = 1000 Hz ;
        MHz = 1000 kHz ;
        GHz = 1000 MHz ;
        THz = 1000 GHz ;
    end units ;

    function half_period(freq : frequency) return time ;
    function period(freq : frequency) return time ;

    procedure generate_clock generic (
        type t ;
        function "not"(x : t) return t is <>
    ) parameter (signal clock : inout t ; freq : frequency ; count : natural := 0) ;

    procedure generate_clock is new generate_clock generic map(t => bit) ;

end package ;
