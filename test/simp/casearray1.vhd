entity casearray1 is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture test of casearray1 is
    signal x : std_logic_vector(7 downto 0);
    signal y : integer;
begin

    p1: process is
        constant b : boolean_vector := not (true, false);
    begin
        case x is
            when X"1" & X"2" => y <= 1;
            when X"1" & not X"2" => y <= 2;
            when X"12" or X"52" => y <= 3;
            when std_logic_vector(to_unsigned(42, 8)) => y <= 4;
            when others => null;
        end case;
        wait;
    end process;

end architecture;
