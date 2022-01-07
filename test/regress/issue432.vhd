library ieee;
use     ieee.std_logic_1164.all;
use     std.textio.all;
entity  SYNC_DRIVER is
    generic (NAME : STRING := ""; DELAY : time := 10 ns; EOT : time := 10 ns);
    port    (SYNC : inout std_logic);
end     SYNC_DRIVER;
architecture MODEL of SYNC_DRIVER is
begin
    process begin
        SYNC   <= 'Z';
        wait for 0 ns;
        SYNC   <= '0';
        wait for DELAY;
        SYNC   <= 'H';
        assert FALSE       report NAME & " WAIT SYNC START" severity NOTE;
        wait until (SYNC = '1' or SYNC = 'H');
        SYNC   <= '0';
        assert FALSE       report NAME & " WAIT SYNC DONE " severity NOTE;
        assert (Now = EOT) report NAME & " SYNC TIME ERROR" severity ERROR;
        wait;
    end process;
end MODEL;

library ieee;
use     ieee.std_logic_1164.all;
entity  SYNC_A is port(SYNC: inout std_logic); end entity;
architecture MODEL of SYNC_A is
begin
    U: entity WORK.SYNC_DRIVER generic map (string'("SYNC_A"), 10 ns, 30 ns) port map(SYNC);
end MODEL;

library ieee;
use     ieee.std_logic_1164.all;
entity  SYNC_B is port(SYNC: inout std_logic); end entity;
architecture MODEL of SYNC_B is
begin
    U: entity WORK.SYNC_DRIVER generic map (string'("SYNC_B"), 20 ns, 30 ns) port map(SYNC);
end MODEL;

library ieee;
use     ieee.std_logic_1164.all;
entity  SYNC_C is port(SYNC: inout std_logic); end entity;
architecture MODEL of SYNC_C is
begin
    U: entity WORK.SYNC_DRIVER generic map (string'("SYNC_C"), 30 ns, 30 ns) port map(SYNC);
end MODEL;

library ieee;
use     ieee.std_logic_1164.all;
entity  issue432 is
end     issue432;
architecture MODEL of issue432 is
    signal  SYNC  : std_logic;
begin
    U_SYNC_A: entity WORK.SYNC_A port map(SYNC);
    U_SYNC_B: entity WORK.SYNC_B port map(SYNC);
    U_SYNC_C: entity WORK.SYNC_C port map(SYNC);
end MODEL;
