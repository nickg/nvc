library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sub is
    port (
        result : out std_ulogic_vector(3 downto 0);
        in1    : in  std_ulogic_vector(3 downto 0) );
end entity;

architecture test of sub is
    signal in2 : std_ulogic_vector(2 downto 0);
begin

    assert in1(1 downto 0) = "00";

    in2 <= "001";
    result <= std_ulogic_vector(unsigned(in1) + unsigned(in2));

end architecture;

-------------------------------------------------------------------------------

entity ieee6 is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture test of ieee6 is
    signal result : std_ulogic_vector(3 downto 0);
    signal in1    : std_ulogic_vector(1 downto 0);
begin

    uut: entity work.sub
        port map (
            result => result,
            in1(3 downto 2) => std_ulogic_vector(in1),
            in1(1 downto 0) => "00" );

    stim: process is
    begin
        in1 <= "01";
        wait for 1 ns;
        assert result = X"5";
        wait;
    end process;

end architecture;
