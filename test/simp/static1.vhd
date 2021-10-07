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

use work.util.all;

entity memory is
    generic (
        WIDTH : integer;
        DEPTH : integer );
    port (
        clk  : in bit;
        addr : in bit_vector(log2(DEPTH) - 1 downto 0);
        din  : in bit_vector(WIDTH - 1 downto 0);
        dout : out bit_vector(WIDTH - 1 downto 0);
        we   : in bit );
end entity;

architecture rtl of memory is
    type ram_t is array (0 to DEPTH - 1) of bit_vector(WIDTH - 1 downto 0);

    signal addr_r : bit_vector(log2(DEPTH) - 1 downto 0);  -- Should be folded
    signal ram    : ram_t;
begin

end architecture;

-------------------------------------------------------------------------------

use work.util.all;

entity bigram is
end entity;

architecture test of bigram is
    constant ITERS : integer := 100;
    constant WIDTH : integer := 1024;
    constant DEPTH : integer := 1024;

    signal clk  : bit := '0';
    signal addr : bit_vector(log2(DEPTH) - 1 downto 0);  -- Should be folded
    signal din  : bit_vector(WIDTH - 1 downto 0);
    signal dout : bit_vector(WIDTH - 1 downto 0);
    signal we   : bit := '1';

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

end architecture;
