entity mixed6 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of mixed6 is
    component sub is
        port (x : out std_logic_vector(7 downto 0));
    end component;

    signal x : std_logic_vector(7 downto 0);
begin
    u: component sub port map (x);

    process is
    begin
        wait for 1 fs;                  -- Iteration 0
        assert x = X"00";
        assert not x'event;
        wait for 0 ns;                  -- Iteration 1
        assert x = X"00";
        assert not x'event;
        wait for 0 ns;                  -- Iteration 2
        assert x = X"01";
        assert x'event;
        wait for 0 ns;                  -- Iteration 3
        assert x = X"01";
        assert not x'event;
        wait for 0 ns;                  -- Iteration 4
        assert x = X"02";
        assert x'event;
        wait for 0 ns;                  -- Iteration 5
        assert x = X"02";
        assert not x'event;
        wait for 0 ns;                  -- Iteration 6
        assert x = X"03";
        assert x'event;
        wait for 0 ns;                  -- Iteration 7
        assert x = X"03";
        assert not x'event;
        wait for 0 ns;                  -- Iteration 8
        assert x = X"04";
        assert x'event;
        wait;
    end process;

    postponed process is
    begin
        wait for 1 fs;
        report "postponed";
        assert x = X"05";
        wait;
    end process;

    process (x) is
    begin
        report "x changed ==> " & to_string(x);
    end process;

end architecture;
