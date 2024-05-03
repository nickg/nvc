library ieee;
use ieee.std_logic_1164.all;

entity first is
    port (
        a : in std_logic_vector(1 downto 0);
        b : out std_logic_vector(1 downto 0)
    );
end entity;

architecture rtl of first is
    signal c : std_logic;

begin
    b <= a;
    c <= a(0);

end architecture;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity second is
    port (
        a : in std_logic_vector(1 downto 0);
        b : out std_logic_vector(1 downto 0) := "00"
    );
end entity;

architecture rtl of second is
begin
    b <= a;
end architecture;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity issue885 is
end entity;

architecture arch of issue885 is
    signal b : std_logic_vector(1 downto 0);
    signal a : std_logic_vector(1 downto 0);

begin
    first : entity work.first(rtl)
        port map(
            b => b,
            a => a
        );

    second : entity work.second(rtl)
        port map(
            a => b,
            b => a
        );

end architecture;
