entity issue228 is
    generic (G : integer range 0 to 3);
end entity issue228;
use std.textio.all;
architecture test of issue228 is
begin
    process
    begin
        write(OUTPUT, integer'image(G) & LF);
        wait;
    end process;
end architecture test;
