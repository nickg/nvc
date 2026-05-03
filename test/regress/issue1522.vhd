library ieee;
use ieee.std_logic_1164.all;

entity signal_checker_v2 is
    port(
        value : in std_logic_vector
    );
end entity signal_checker_v2;

architecture tb of signal_checker_v2 is
begin
    process is
    begin
        assert value = x"ABCDEF01";
        wait;
    end process;
end architecture tb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue1522 is
end entity issue1522;

architecture tb of issue1522 is
    signal Data : unsigned(31 downto 0) := x"ABCDEF01";
begin

    i_test : entity work.signal_checker_v2
    port map(
        value(Data'range) => std_logic_vector(Data)
    );

end architecture tb;
