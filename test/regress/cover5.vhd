
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cover5 is
    generic (
        G_VAL : integer
    );
end entity;

architecture test of cover5 is

    signal vect : std_logic_vector(15 downto 0) := x"AAAA";

    -- Needed to pass via signal to avoid optimizing out parts of
    -- the if/else conditions.
    signal cnt : integer := G_VAL;

begin

    -- Check on statements and branches
    process(cnt)
    begin
        l_if1: if (cnt = 1) then report "IF1: cnt = 1";
        elsif (cnt = 2) then     report "IF1: cnt = 2";
        elsif (cnt = 3) then     report "IF1: cnt = 3";
        else                     report "IF1: cnt = others";
        end if;
    end process;

    -- Check on toggles
    process
    begin
        wait for 1 ns;
        for i in 1 to G_VAL loop
            vect(i * G_VAL - 1) <= not vect(i * G_VAL - 1);
        end loop;
        wait for 1 ns;
        wait;
    end process;

    -- Check with different amount of statements in each branch
    -- to get different coverage results for each run!
    process(cnt)
    begin
        l_if2: if (cnt = 1) then
            report "IF2: cnt = 1/1";
        elsif (cnt = 2) then
            report "IF2: cnt = 1/2";
            report "IF2: cnt = 2/2";
        elsif (cnt = 3) then
            report "IF2: cnt = 1/3";
            report "IF2: cnt = 2/3";
            report "IF2: cnt = 3/3";
        elsif (cnt = 4) then
            report "IF2: cnt = 1/4";
            report "IF2: cnt = 2/4";
            report "IF2: cnt = 3/4";
            report "IF2: cnt = 4/4";
        end if;
    end process;

end architecture;
