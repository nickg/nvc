library ieee;
use ieee.std_logic_1164.all;

entity issue202 is
end entity;

architecture a of issue202 is
  type rec_t is record
    field : std_logic;
    field2 : std_logic_vector(1 downto 0);
  end record;

  signal sig : rec_t;
begin

  p1 : process
  begin
    sig <= ('0', "0Z");
    wait;
  end process;

  p2 : process
  begin
    sig <= ('1', "10");
    wait;
  end process;

  monitor : process (sig)
  begin
    report std_logic'image(sig.field);
    report std_logic'image(sig.field2(1)) & std_logic'image(sig.field2(0));
  end process;

end architecture;
