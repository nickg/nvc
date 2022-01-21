library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fifo is
    generic (
        type T;
        DEPTH : positive );
    port (
        clk   : in std_logic;
        pop   : in std_logic;
        push  : in std_logic;
        full  : out std_logic;
        valid : out std_logic;
        din   : in T;
        dout  : out T );
end entity;

architecture test of fifo is
    type fifo_data_t is array (0 to DEPTH - 1) of T;

    signal wptr, rptr : natural := 1;
    signal data       : fifo_data_t;
begin

    full  <= '1' when (wptr + 1) mod DEPTH = rptr else '0';
    valid <= '1' when wptr /= rptr else '0';
    dout  <= data(rptr);

    behav: process (clk) is
    begin
        if rising_edge(clk) then
            if push = '1' then
                assert full = '0';
                data(wptr) <= din;
                wptr <= (wptr + 1) mod DEPTH;
            end if;
            if pop = '1' then
                assert valid = '1';
                rptr <= (rptr + 1) mod DEPTH;
            end if;
        end if;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity gentype1 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of gentype1 is
    signal clk : std_logic;
    signal pop : std_logic;
    signal push : std_logic;
    signal din  : integer;
    signal dout : integer;
    signal full : std_logic;
    signal valid : std_logic;

    procedure pulse (signal clk : out std_logic) is
    begin
        clk <= '0';
        wait for 1 ns;
        clk <= '1';
        wait for 1 ns;
        clk <= '0';
    end procedure;
begin

    u: entity work.fifo
        generic map ( T => integer, depth => 4 )
        port map (
            clk  => clk,
            pop  => pop,
            push => push,
            din  => din,
            dout => dout,
            full => full,
            valid => valid );

    main: process is
    begin
        wait for 1 ns;
        assert full = '0';
        assert valid = '0';

        din <= 5;
        push <= '1';
        pulse(clk);

        assert full = '0';
        assert valid = '1';
        assert dout = 5;

        din <= 7;
        pulse(clk);

        assert full = '0';
        assert valid = '1';
        assert dout = 5;

        push <= '0';
        pop <= '1';
        pulse(clk);

        assert full = '0';
        assert valid = '1';
        assert dout = 7;

        pulse(clk);

        assert full = '0';
        assert valid = '0';

        wait;
    end process;

end architecture;
