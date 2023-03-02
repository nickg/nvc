entity file8 is
end entity;

use std.textio.all;

architecture test of file8 is
begin
    p1: process is
        file F: text open read_mode is "NOT_HERE.txt";  -- Error
    begin
        wait for 1 ns;
        wait;
    end process;
end architecture;
