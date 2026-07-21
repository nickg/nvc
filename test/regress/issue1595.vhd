library ieee;
use ieee.fixed_pkg.all;

entity issue1595 is
end entity;

architecture test of issue1595 is
    type array_sfixed_t is array (natural range <>) of sfixed;
    signal theta_table : array_sfixed_t(0 to 15)(3 downto -28);
begin
end architecture;
