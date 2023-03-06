library ieee;
    use ieee.std_logic_1164.all;

entity dff is
  generic (
    function edge(signal s : std_ulogic) return boolean
  ) ;
  port (
    clock : in  std_ulogic ;
    d     : in  std_ulogic ;
    q     : out std_ulogic
  ) ;
end entity ;

architecture arch of dff is

begin

    process(clock)
    begin
        if edge(clock) then
            q <= d ;
        end if ;
    end process ;

end architecture ;

library ieee ;
    use ieee.std_logic_1164.all;

entity dff_top is
  port (
    clock   :   in  std_ulogic ;
    d       :   in  std_ulogic_vector(1 downto 0) ;
    q       :   out std_ulogic_vector(1 downto 0)
  ) ;
end entity ;

architecture arch of dff_top is

begin

    U_dff0 : entity work.dff
      generic map (
        edge        => falling_edge
      ) port map (
        clock       =>  clock,
        d           =>  d(0),
        q           =>  q(0)
      ) ;

    U_dff1 : entity work.dff
      generic map (
        edge        =>  rising_edge
      ) port map (
        clock       =>  clock,
        d           =>  d(1),
        q           =>  q(1)
      ) ;

end architecture ;

library ieee ;
    use ieee.std_logic_1164.all;

entity issue620 is
end entity;

architecture test of issue620 is
    signal clock   :   std_ulogic := '0';
    signal d       :   std_ulogic_vector(1 downto 0) ;
    signal q       :   std_ulogic_vector(1 downto 0);
begin

    u: entity work.dff_top
        port map ( clock, d, q );

    stim: process is
    begin
        d <= "11";
        wait for 1 ns;
        clock <= '1';
        wait for 1 ns;
        assert q = "1U";
        clock <= '0';
        wait for 1 ns;
        assert q = "11";
        d <= "01";
        clock <= '1';
        wait for 1 ns;
        assert q = "01";

        wait;
    end process;

end architecture;
