
library ieee;
use ieee.std_logic_1164.all;

entity toggle_cov is
end entity;

architecture tb of toggle_cov is

    constant C_MEM_DEPTH : natural := 2 ** 18;

    type t_large_mem is
        array (0 to C_MEM_DEPTH - 1) of std_logic_vector(31 downto 0);

    signal large_mem : t_large_mem := (others => (others => '0'));

begin

    process
    begin

        wait for 1 ns;

        for j in 1 to 500 loop

            report "Iteration: " & integer'image(j);

            for i in 0 to C_MEM_DEPTH - 1 loop
                large_mem(i) <= "00000000000000000000000000000000";
            end loop;

            wait for 1 ns;

            for i in 0 to C_MEM_DEPTH - 1 loop
                large_mem(i) <= "11111111111111111111111111111111";
            end loop;

            wait for 1 ns;

            for i in 0 to C_MEM_DEPTH - 1 loop
                large_mem(i) <= "00000000000000000000000000000000";
            end loop;

            wait for 1 ns;

            for i in 0 to C_MEM_DEPTH - 1 loop
                large_mem(i) <= "11111111111111111111111111111111";
            end loop;

            wait for 1 ns;
        end loop;

        wait;
    end process;

end architecture;

