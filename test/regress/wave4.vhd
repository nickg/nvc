entity wave4 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of wave4 is
    type mem_t is array (natural range <>) of std_logic_vector(7 downto 0);

    signal mem : mem_t(0 to 9);
    signal dout : std_logic_vector(7 downto 0);
    signal addr : natural range 0 to 7;
begin

    dout <= mem(addr);

    main: process is
    begin
        mem(2) <= X"01";
        mem(5) <= X"55" after 1 ns;
        addr <= 5 after 10 ns;
        wait;
    end process;

end architecture;
