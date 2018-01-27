library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.math_real.all;

entity issue293 is
end issue293;

architecture behv of issue293 is

  constant AWIDTH : natural := integer(ceil(log2(real(4))));
  signal a : std_logic_vector (AWIDTH downto 0);

begin

    process is
    begin
        assert a'left = 2;
        wait;
    end process;

end behv;
