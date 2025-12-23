entity vlog29 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

use std.textio.all;

architecture test of vlog29 is
    component sub is
        port (
            load : in std_logic;
            addr : in std_logic_vector(4 downto 0);
            dout : out std_logic_vector(15 downto 0) );
    end component;

    signal load : std_logic := '0';
    signal addr : std_logic_vector(4 downto 0);
    signal dout : std_logic_vector(15 downto 0);
begin

    u: component sub
        port map ( load, addr, dout );

    process is
        file f : text;
        variable l : line;
    begin
        file_open(f, "data.hex", write_mode);
        swrite(l, "dead");
        writeline(f, l);
        swrite(l, "beef");
        writeline(f, l);
        file_close(f);

        load <= '1';
        wait for 0 ns;

        addr <= "00000";
        wait for 0 ns;
        wait for 0 ns;                  -- TODO: Questa only needs one delta here
        assert dout = X"dead" report to_hstring(dout);
        addr <= "00001";
        wait for 0 ns;
        wait for 0 ns;
        assert dout = X"beef" report to_hstring(dout);

        wait;
    end process;

end architecture;
