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

    procedure generate_clock is new generate_clock  -- OK
        generic map(t => bit) ;

    procedure bad1 is new generate_clock [return integer];  -- Error
    procedure bad2 is new "+";  -- Error

    procedure generate_clock generic (
        type t ;
        function "not"(x : t) return t is <>
    ) parameter (signal clock : inout t) ;

    procedure bad3 is new generate_clock;  -- Error

    procedure test1 generic (type t) (x : t);

    procedure test1 is new test1 generic map (integer);  -- OK
end package ;

package body frequency is
    procedure test1 generic (type t) (x : t) is
    begin
        generate_clock(bit, 1 Hz);      -- Error
    end procedure;
end package body;
