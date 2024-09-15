entity ieee17 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of ieee17 is
begin

    -- Corner cases for "="
    process is
        variable v1, v2 : std_logic_vector(50 downto 0);
    begin
        for i in 0 to 50 loop
            for j in 0 to i - 1 loop
                v1(j) := std_logic'val(j mod 9);
                v2(j) := std_logic'val(j mod 9);
            end loop;
            v1(i) := 'X';
            v2(i) := 'U';
            assert v1(i - 1 downto 0) = v2(i - 1 downto 0);
            assert not (v1(i downto 0) = v2(i downto 0));
        end loop;
        wait;
    end process;

end architecture;
