
library ieee;
use ieee.std_logic_1164.all;

entity issue1442 is
end entity;

architecture test of issue1442 is

    type t_deep is array (
        0 to 0,
        0 to 0,
        0 to 0,
        0 to 0,
        0 to 0,
        0 to 0,
        0 to 0,
        0 to 0,
        0 to 0,
        0 to 0,
        0 to 0,
        0 to 0,
        0 to 0,
        0 to 0,
        0 to 0,
        0 to 0,
        0 to 0,
        0 to 0
    ) of std_logic;
    signal s : t_deep;
begin
end architecture;