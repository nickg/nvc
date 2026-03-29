entity mixed9 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of mixed9 is
    component sub is
        port (i : in std_logic_vector(7 downto 0);
              o : out std_logic_vector(7 downto 0));
    end component;

    signal i, o : std_logic_vector(7 downto 0);
begin
    u: component sub port map (i, o);

    check: process is
    begin
        wait for 1 fs;                  -- Iteration 0
        i <= X"01";
        wait for 0 ns;                  -- Iteration 1
        assert o = X"02";
        wait;
    end process;

    monitor: process (o) is
    begin
        report "o changed ==> " & to_string(o);
    end process;

end architecture;
