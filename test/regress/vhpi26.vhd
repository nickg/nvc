library ieee;
use ieee.std_logic_1164.all;

entity vhpi26 is
end entity;

architecture test of vhpi26 is
    function get_one return integer is
    begin
        assert false report "foreign binding was not resolved" severity failure;
    end function;

    attribute foreign : string;
    attribute foreign of get_one : function is "VHPI vhpi26 get_one";

    signal sl : std_logic;
begin
    process is
    begin
        wait for 0 ns;
        assert get_one = 1;             -- Requires vhpi_test plugin
        assert sl = '1';                -- Requires siginit plugin
        wait;
    end process;
end architecture;
