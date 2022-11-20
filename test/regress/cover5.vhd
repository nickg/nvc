
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cover5 is
    generic (
        G_VAL : integer
    );
end entity;

architecture test of cover5 is

    signal vect : std_logic_vector(4 downto 0) := "10101";

    -- Needed to pass via signal to avoid optimizing out parts of
    -- the if/else conditions.
    signal cnt : integer := G_VAL;

begin

    -- Check on statements and branches
    process(cnt)
    begin
        l_if1: if (cnt = 1) then report "IF1: cnt = 0";
        elsif (cnt = 2) then     report "IF1: cnt = 2";
        elsif (cnt = 3) then     report "IF1: cnt = 3";
        else                     report "IF1: cnt = others";
        end if;
    end process;

    -- Check on toggles
    process
    begin
        wait for 1 ns;
        vect(G_VAL) <= not vect(G_VAL);
        wait for 1 ns;
        wait;
    end process;

end architecture;
