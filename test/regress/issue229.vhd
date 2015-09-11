use std.textio.all;
entity issue229 is
end entity issue229;
architecture test of issue229 is
begin
    process
    begin
        for ii in 1 to 2049 loop
            write(OUTPUT, integer'image(1));
        end loop;
        wait;
    end process;
    -- Alternatively, delete the for loop and the wait statement -- it fails the same way
end architecture test;
