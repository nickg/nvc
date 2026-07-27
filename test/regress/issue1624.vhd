library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package issue1624_pkg is
    type array_of_signed is array (integer range <>) of signed;

    type interlaced_signal is record
        data  : array_of_signed;
        valid : std_logic;
    end record;
end package;

library ieee;
use ieee.std_logic_1164.all;

use work.issue1624_pkg.all;

entity issue1624 is
    generic (
        in_lanes : integer := 1
    );
    port (
        clk     : in std_logic;
        data_in : in interlaced_signal(data(0 to in_lanes - 1)(31 downto 0))
    );
end entity;

architecture test of issue1624 is
    signal level_main : std_logic_vector(in_lanes - 1 downto 0);
begin
    level_main_gen: for l in 0 to in_lanes - 1 generate
        level_main(l) <= data_in.data(l)(31);
    end generate;
end architecture;
