library ieee;
use ieee.std_logic_1164.all;

entity vlog141 is
end entity;

architecture sim of vlog141 is
   component vlog141_mid is
      port (mirror : out std_logic_vector(7 downto 0);
            tag    : in  std_logic_vector(3 downto 0));
   end component;

   signal m_a, m_b : std_logic_vector(7 downto 0);
begin
   u_a : vlog141_mid port map (mirror => m_a, tag => "0100");
   u_b : vlog141_mid port map (mirror => m_b, tag => "0111");

   check : process is
   begin
      wait for 1 ns;
      assert m_a = "01000100" report "FAIL m_a" severity failure;
      assert m_b = "01110111" report "FAIL m_b" severity failure;
      report "PASSED";
      wait;
   end process;
end architecture;
