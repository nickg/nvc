package util is

    function log2(x : in integer) return integer;

end package;

package body util is

    function log2(x : in integer) return integer is
        variable r : integer := 0;
        variable c : integer := 1;
    begin
        if x <= 1 then
            r := 1;
        else
            while c < x loop
                r := r + 1;
                c := c * 2;
            end loop;
        end if;
        return r;
    end function;

end package body;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.util.all;

entity memory is
    generic (
        WIDTH : integer;
        DEPTH : integer );
    port (
        clk  : in std_logic;
        addr : in unsigned(log2(DEPTH) - 1 downto 0);
        din  : in std_logic_vector(WIDTH - 1 downto 0);
        dout : out std_logic_vector(WIDTH - 1 downto 0);
        we   : in std_logic );
end entity;

architecture rtl of memory is
    type ram_t is array (0 to DEPTH - 1) of std_logic_vector(WIDTH - 1 downto 0);

    signal addr_r : unsigned(log2(DEPTH) - 1 downto 0);
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

use work.util.all;

entity bigram is
end entity;

architecture test of bigram is
    constant ITERS : integer := 100;
    constant WIDTH : integer := 1024;
    constant DEPTH : integer := 1024;

    signal clk  : std_logic := '0';
    signal addr : unsigned(log2(DEPTH) - 1 downto 0);
    signal din  : std_logic_vector(WIDTH - 1 downto 0);
    signal dout : std_logic_vector(WIDTH - 1 downto 0);
    signal we   : std_logic := '1';

    signal running : boolean := true;

begin

    clk <= not clk after 5 ns when running else '0';

    uut: entity work.memory
        generic map (
            WIDTH => WIDTH,
            DEPTH => DEPTH )
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
            for i in 0 to DEPTH - 1 loop
                addr <= to_unsigned(i, addr'length);
                din  <= std_logic_vector(to_unsigned((i + j) mod DEPTH, WIDTH));
                wait for 10 ns;
            end loop;
            we <= '0';
            for i in 0 to DEPTH - 1 loop
                addr <= to_unsigned(i, addr'length);
                wait for 10 ns;
                assert dout = std_logic_vector(to_unsigned((i + j) mod DEPTH, WIDTH));
            end loop;
        end loop;
        running <= false;
        wait;
    end process;

end architecture;
