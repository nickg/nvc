entity issue1516 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

use std.textio.all;

architecture test of issue1516 is
    component issue1516_sub is
        port (
            load : in std_logic;
            addr : in std_logic_vector(1 downto 0);
            dout : out std_logic_vector(49 downto 0) );
    end component;

    signal load : std_logic := '0';
    signal addr : std_logic_vector(1 downto 0);
    signal dout : std_logic_vector(49 downto 0);
begin

    u: component issue1516_sub
        port map ( load, addr, dout );

    process is
        file f : text;
        variable l : line;
    begin
        file_open(f, "issue1516.hex", write_mode);
        swrite(l, "1000000000000");
        writeline(f, l);
        swrite(l, "0b504f5a57d86");
        writeline(f, l);
        swrite(l, "0000001800000");
        writeline(f, l);
        swrite(l, "34afb0da57d86");
        writeline(f, l);
        file_close(f);

        load <= '1';
        wait for 0 ns;

        addr <= "00";
        wait for 0 ns;
        assert to_hstring(dout) = "1000000000000" report to_hstring(dout);

        addr <= "01";
        wait for 0 ns;
        assert to_hstring(dout) = "0B504F5A57D86" report to_hstring(dout);

        addr <= "10";
        wait for 0 ns;
        assert to_hstring(dout) = "0000001800000" report to_hstring(dout);

        addr <= "11";
        wait for 0 ns;
        assert to_hstring(dout) = "34AFB0DA57D86" report to_hstring(dout);

        wait;
    end process;

end architecture;
