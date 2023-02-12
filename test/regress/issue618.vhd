library ieee ;
use ieee.std_logic_1164.all ;

entity issue618 is
  generic (
    A    : std_logic := '0' ;
    B      : std_logic := '1'
  ) ;
end entity;

architecture test of issue618 is
begin
    p1: process is
    begin
        assert A = '1';
        assert B = '0';
        wait;
    end process;
end architecture;
