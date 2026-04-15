library ieee;
use ieee.std_logic_1164.all;

entity merge2 is
    generic (
        G_VAL : integer
    );
end merge2;

architecture test of merge2 is
    signal tgl : std_logic_vector(G_VAL downto 0) := (others => '0');
begin

    process
    begin
        tgl(0) <= '1';
        wait for 1 ns;
        tgl(0) <= '0';
        wait for 1 ns;

        tgl(1) <= '1';
        wait for 1 ns;
        tgl(1) <= '0';
        wait for 1 ns;

        wait;
    end process;

end architecture;
