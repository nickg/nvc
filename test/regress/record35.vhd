-- Test case from Brian Padalino
--

library ieee;
use ieee.std_logic_1164.all;

package abc is
    type Parameters_t is record
        BW    : natural;
        PAIRS : natural;
    end record;

    type Indices_t is array (natural range <>) of std_logic_vector;

    type Bus_t is record
        Indices : Indices_t;
    end record;

    function Test(
        abc_bus : Bus_t
    ) return Bus_t;

    function Test(
        abc_bus : Bus_t;
        indices : Indices_t
    ) return Bus_t;
end package;

package body abc is
    function Test(
        abc_bus : Bus_t;
        indices : Indices_t
    ) return Bus_t is
        variable result : Bus_t(
            Indices(abc_bus.Indices'range)(abc_bus.Indices'element'range)
        ) := Test(abc_bus);
    begin
        return result;
    end function;

    function Test(
        abc_bus : Bus_t
    ) return Bus_t is
        variable result : Bus_t(
            Indices(abc_bus.Indices'range)(abc_bus.Indices'element'range)
        ) := abc_bus;
    begin
        return result;
    end function;
end package body;
library ieee;
use ieee.std_logic_1164.all;

use work.abc;
use work.abc.all;

use std.env.finish;

entity record35 is
end entity;

architecture sim of record35 is
    constant CLK_PERIOD : time := 10 ns;

    signal clk : std_ulogic := '0';

    constant abc_BUS_SETTINGS : abc.Parameters_t := (
        BW    => 8,
        PAIRS => 2
    );

    signal abc_bus : abc.Bus_t(
        Indices(abc_BUS_SETTINGS.PAIRS - 1 downto 0)(abc_BUS_SETTINGS.BW - 1 downto 0)
    );

begin
    clk <= not clk after CLK_PERIOD / 2;
    test_runner : process
    begin
		abc_bus <= abc.Test(
			abc_bus,
			(
				0 => std_logic_vector'(x"00"),
				1 => std_logic_vector'(x"01")
			)
		);
		wait for CLK_PERIOD;
		finish;
    end process test_runner;

end architecture sim;
