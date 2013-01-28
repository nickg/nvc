entity signal9 is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture test of signal9 is
    signal vec : std_logic_vector(7 downto 0);
begin

    assign_p: vec <= X"52";

    count_p: process is
        variable ctr : unsigned(7 downto 0) := X"00";
    begin
        wait for 1 ns;
        loop
            ctr := ctr + 1;
            exit when vec = std_logic_vector(ctr);
        end loop;
        loop
            ctr := ctr + 1;
            exit when unsigned(vec) = ctr;
        end loop;
        wait;
    end process;

end architecture;
