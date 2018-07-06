library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity issue377 is
end entity;

architecture test of issue377 is
    signal sticky : std_logic;
    signal shiftedFracY_d1 :  std_logic_vector(49 downto 0);
begin
    update: sticky <= '0' when (shiftedFracY_d1(23 downto 0)=CONV_STD_LOGIC_VECTOR(0,23)) else '1';

    stim: process is
    begin
        wait for 1 ns;
        assert sticky = '1';
        shiftedFracY_d1(19 downto 0) <= X"00000";
        wait for 1 ns;
        assert sticky = '1';
        shiftedFracY_d1(24 downto 20) <= "00000";
        wait for 1 ns;
        assert sticky = '0';
        wait;
    end process;

end architecture;
