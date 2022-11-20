
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cover4 is
end entity;

architecture test of cover4 is

    signal cnt      : integer := 0;
    signal res_1    : std_logic_vector(3 downto 0);
    signal res_2    : std_logic_vector(3 downto 0);

begin

    -- Test control
    process
    begin
        wait for 1 ns;
        if (cnt < 20) then
            cnt <= cnt + 1;
        else
            wait for 1 ns;
            wait;
        end if;
    end process;

    -- When else
    res_1 <=
        "0000" when (cnt = 0) else
        "0001" when (cnt = 1) else
        -- coverage off
        "0010" when (cnt = 2) else
        -- coverage on
        "0011" when (cnt = 3) else
        "0100" when (cnt = 4) else
        "0101" when (cnt = 5) else
        "0111" when (cnt = 6) else
        "XXXX";

    -- With select
    with (cnt) select res_2 <=
        "0000" when 0,
        "0001" when 1,
        "0010" when 2,
        "0011" when 3,
        -- coverage off
        "0100" when 4,
        -- coverage on
        "0101" when 5,
        "0111" when 6,
        "XXXX" when others;

end architecture;
