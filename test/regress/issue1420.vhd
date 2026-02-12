library ieee;
use ieee.std_logic_1164.all;

package repro_pkg is
    -- Unconstrained array type used for generic mapping and for a port.
    type t_map is array (natural range <>) of std_logic_vector(7 downto 0);

    -- A record type to make the port an array of records (mirrors the real case).
    type t_rec is record
        data : std_logic_vector(7 downto 0);
        flag : std_logic;
    end record t_rec;

    type t_rec_array is array (natural range <>) of t_rec;

    -- Empty (zero-length) array constant.
    constant c_empty_map : t_map(0 to -1) := (0 to -1 => (others => '0'));
end package repro_pkg;

package body repro_pkg is
end package body repro_pkg;

library ieee;
use ieee.std_logic_1164.all;
use work.repro_pkg.all;

entity repro_unit is
    generic (
        -- Default generic is the empty array.
        g_map : t_map := c_empty_map
    );
    port (
        -- This port becomes a zero-length array when g_map is empty.
        out_o : out t_rec_array(g_map'range)
    );
end entity repro_unit;

architecture rtl of repro_unit is
begin
end architecture rtl;

library ieee;
use ieee.std_logic_1164.all;
use work.repro_pkg.all;

entity issue1420 is
end entity;

architecture sim of issue1420 is
    -- Signal with a zero-length array range.
    signal out_s : t_rec_array(c_empty_map'range);
    signal clk   : std_logic := '0';
begin
    clk <= not clk after 5 ns;

    dut : entity work.repro_unit
        port map (
            -- out_s is zero-length; dumping arrays with NVC triggers the crash.
            out_o => out_s
        );

    process is
    begin
        wait for 50 ns;
        std.env.finish;
    end process;
end architecture sim;
