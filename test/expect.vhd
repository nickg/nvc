library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity memory is
    generic (
        WIDTH : integer );
    port (
        clk  : in std_logic;
        addr : in unsigned(7 downto 0);
        din  : in std_logic_vector(WIDTH - 1 downto 0);
        dout : out std_logic_vector(WIDTH - 1 downto 0);
        we   : in std_logic );
end entity;

architecture rtl of memory is
    type ram_t is array (0 to 255) of std_logic_vector(WIDTH - 1 downto 0);

    signal addr_r : unsigned(7 downto 0) := X"00";
    signal ram    : ram_t;
begin

    reg: process (clk) is
    begin
        if rising_edge(clk) then
            addr_r <= addr;
            if we = '1' then
                ram(to_integer(addr)) <= din;
            end if;
        end if;
    end process;

    dout <= ram(to_integer(addr_r));

end architecture;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top is
end entity;

architecture test of top is
    constant ITERS : integer := 1;
    constant WIDTH : integer := 8;

    signal clk  : std_logic := '0';
    signal addr : unsigned(7 downto 0) := X"00";
    signal din  : std_logic_vector(WIDTH - 1 downto 0);
    signal dout : std_logic_vector(WIDTH - 1 downto 0);
    signal we   : std_logic := '0';

    signal running : boolean := true;
begin

    clk <= not clk after 5 ns when running else '0';

    uut: entity work.memory
        generic map (
            WIDTH => WIDTH )
        port map (
            clk  => clk,
            addr => addr,
            din  => din,
            dout => dout,
            we   => we );

    stim: process is
    begin
        for j in 1 to ITERS loop
            wait for 20 ns;
            we <= '1';
            for i in 0 to 255 loop
                addr <= to_unsigned(i, 8);
                din  <= std_logic_vector(to_unsigned((i + j) mod 256, WIDTH));
                wait for 10 ns;
            end loop;
            we <= '0';
            for i in 0 to 255 loop
                addr <= to_unsigned(i, 8);
                wait for 10 ns;
                assert dout = std_logic_vector(to_unsigned((i + j) mod 256, WIDTH));
            end loop;
        end loop;
    end process;

end architecture;
