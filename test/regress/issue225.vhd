-- use std.textio.all;                          -- this does work
entity issue225 is
    -- use std.textio.all;                         -- doesn't work
end entity issue225;
-- use std.textio.all;                          -- this does work
architecture test of issue225 is
    use std.textio.all;                      -- doesn't work
begin
    g1: if true generate
        -- use std.textio.all;                  -- doesn't work (this one doesn't compile unless parse.c's p_generate_statement's call to scan() is amended to include pUSE (issue 223)
    begin
        b1: block
            -- use std.textio.all;              -- doesn't work
        begin
            p1: process
                -- use std.textio.all;          -- doesn't work
                procedure doit is
                    -- use std.textio.all;      -- doesn't work
                begin
                    write(OUTPUT, "ok" & LF);
                    wait;
                end procedure doit;
            begin
                doit;
            end process p1;
        end block b1;
    end generate g1;
end architecture test;
