library ieee;
use ieee.std_logic_1164.all;

entity shift_reg is
    generic (
        WIDTH : positive );
    port (
        clk   : in std_logic;
        reset : in std_logic;

        shift : in  std_logic;
        din   : in  std_logic;
        dout  : out std_logic );
end entity;

architecture rtl of shift_reg is
    signal shift_r : std_logic_vector(WIDTH - 1 downto 0);
begin

    shift_p: process (clk) is
    begin
        if rising_edge(clk) then
            if reset = '1' then
                shift_r <= (others => '-');
            elsif shift = '1' then
                shift_r <= shift_r(WIDTH - 2 downto 0) & din;
            end if;
        end if;
    end process;

    dout <= shift_r(WIDTH - 1);

end architecture;

-------------------------------------------------------------------------------

entity concat3 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of concat3 is
    signal clk     : std_logic := '1';
    signal reset   : std_logic := '1';
    signal shift   : std_logic;
    signal din     : std_logic;
    signal dout    : std_logic;
    signal running : boolean := true;
begin

    clk <= not clk after 10 ns when running;

    reset <= '0' after 30 ns;

    uut: entity work.shift_reg
        generic map (
            WIDTH => 4 )
        port map (
            clk   => clk,
            reset => reset,

            shift => shift,
            din   => din,
            dout  => dout );

    process is
    begin
        shift <= '0';

        wait until reset = '0';
        wait until falling_edge(clk);

        shift <= '1';
        din   <= '0';
        wait until falling_edge(clk);
        wait until falling_edge(clk);
        wait until falling_edge(clk);
        wait until falling_edge(clk);
        assert dout = '0';
        din <= '1';
        wait until falling_edge(clk);
        assert dout = '0';
        wait until falling_edge(clk);
        assert dout = '0';
        wait until falling_edge(clk);
        assert dout = '0';
        wait until falling_edge(clk);
        assert dout = '1';

        running <= false;
        wait;
    end process;

end architecture;
