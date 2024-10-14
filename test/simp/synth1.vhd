entity synth1 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of synth1 is
    signal clk : std_logic;
    signal reset, x, y : bit;
    signal z : bit_vector(1 to 3);

    type t_rec is record
        x : bit;
        y : bit_vector(1 to 3);
    end record;

    signal r : t_rec;
begin

    p1: process (x) is                  -- Warning
    begin
        y <= x and z(1) and y;
    end process;

    p2: process (x, z) is               -- No warning
    begin
        y <= x and z(1);
    end process;

    p3: process (clk) is                -- No warning
    begin
        if clk'event and clk = '1' then
            y <= x;
        end if;
    end process;

    p4: process (x) is                  -- Warning
    begin
        if clk'event and clk = '1' then
            y <= x;
        end if;
    end process;

    p5: process (clk, reset) is         -- No warning
    begin
        if reset = '1' then
            y <= '0';
        elsif clk'event and clk = '1' then
            y <= x;
        end if;
    end process;

    p6: process (clk) is                -- Warning
    begin
        if reset = '1' then
            y <= '0';
        elsif clk'event and clk = '1' then
            y <= x;
        else
            y <= '1';
        end if;
    end process;

    p7: process (clk, reset) is         -- No warning
    begin
        if reset = '1' then
            y <= '0';
        elsif rising_edge(clk) then
            y <= x;
        end if;
    end process;

    p8: process (r.x, r.y) is           -- No warning
    begin
        y <= r.x or r.y(1);
    end process;

    process (r.x) is                    -- Warning
    begin
        y <= r.x or r.y(1);
    end process;

end architecture;
