library ieee;
use ieee.std_logic_1164.all;

entity issue484 is
end entity;

architecture beh of issue484 is
  type t_data is record
    sig1 : std_logic;
    sig2 : std_logic_vector(7 downto 0);
    sig3 : std_logic;
  end record;
  type t_data_array      is array (natural range <>) of t_data;
  signal sr  : t_data_array(0 to 2) := (others => ('1', x"ff", '1'));
begin
  process
  begin
    for i in 1 to 3 loop
      sr <= sr(sr'low + 1 to sr'high) & t_data'('0', x"00", '0');
      wait for 1 ns;
    end loop;
    wait;
  end process;

  process is
  begin
    wait for 1 ns;
    assert sr(0).sig1 = '1';
    assert sr(0).sig2 = x"ff";
    assert sr(2).sig1 = '0';
    assert sr(2).sig2 = x"00";
    wait for 5 ns;
    assert sr(0).sig1 = '0';
    assert sr(0).sig2 = x"00";
    assert sr(2).sig1 = '0';
    assert sr(2).sig2 = x"00";
    wait;
  end process;
end architecture beh;
