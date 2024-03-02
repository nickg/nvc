library ieee ;
    use ieee.std_logic_1164.std_logic ;
    use ieee.std_logic_1164.std_ulogic ;
    use ieee.std_logic_1164."not";

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

    procedure generate_clock is new generate_clock generic map(t => std_ulogic) ;
    --procedure generate_clock is new generate_clock generic map(t => std_logic) ;
    procedure generate_clock is new generate_clock generic map(t => bit) ;

end package ;

package body frequency is

    function period(freq : frequency) return time is
    begin
        return 1 sec / frequency'pos(freq) ;
    end function ;

    function half_period(freq : frequency) return time is
    begin
        return period(freq) / 2.0 ;
    end function ;

    procedure generate_clock generic(
      type t ;
      function "not"(x : t) return t is <>
    ) parameter (
        signal clock : inout t ;
        freq : frequency ;
        count : natural := 0
    ) is
        constant hp : time := half_period(freq) ;
        variable downcount : natural := count ;
    begin
        -- count = 0 means forever, otherwise we look at the downcount
        while count = 0 or downcount > 0 loop
            clock <= not clock;
            wait for hp;
            clock <= not clock;
            wait for hp;
            downcount := downcount - 1 ;
        end loop ;
    end procedure ;

end package body ;

-------------------------------------------------------------------------------

entity issue654 is
end entity;

use work.frequency.all;

architecture test of issue654 is
    signal clk : bit;
begin

    clkgen: process is
    begin
        generate_clock(clk, 1 GHz, 100);
        assert now = 100 ns;
        wait;
    end process;

    check: process is
    begin
        for i in 1 to 100 loop
            wait for 0.5 ns;
            assert clk = '1';
            wait for 0.5 ns;
            assert clk = '0';
        end loop;
        wait;
    end process;

end architecture;
