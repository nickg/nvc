library ieee;
use ieee.std_logic_1164.all;

entity polarfire_test is
end entity;

architecture test of polarfire_test is
    component SLE_Prim is
        port ( Q : out std_logic;
               ADn, ALn, CLK, D, LAT, SD, EN, SLn : in std_logic );
    end component;

    signal Q                                  : std_logic;
    signal ADn, ALn, CLK, D, LAT, SD, EN, SLn : std_logic;
begin

    u: component SLE_Prim
        port map (Q, ADn, ALn, CLK, D, LAT, SD, EN, SLn);

end architecture;
