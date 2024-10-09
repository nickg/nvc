library ieee;
use ieee.std_logic_1164.all;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;

entity sub_block is
    generic (
        G_NUM_ENTRIES : natural
    );
    port (
        addr : in std_logic_vector(natural(ceil(log2(real(G_NUM_ENTRIES)))) - 1 downto 0);
        addrb : out std_logic_vector(0 downto 0)
    );
end entity;

architecture test of sub_block is

begin
    process (addr)
    begin
        report "ADDR changed ";
    end process;
end;



library ieee;
use ieee.std_logic_1164.all;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;

entity cover25 is
end entity;

architecture test of cover25 is

    signal addr : std_logic_vector(0 downto 0);

begin

    i_sub_block : entity work.sub_block
    generic map (
        G_NUM_ENTRIES => 1
    )
    port map (
        addr  => addr
    );

    process
    begin
        wait for 1 ns;
        addr <= "1";
        wait for 1 ns;
        addr <= "0";
        wait;
    end process;

end architecture;
