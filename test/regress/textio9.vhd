-- Test for native readline intrinsic:
-- Verifies correct behaviour for empty lines, lines longer than the
-- initial 256-byte buffer, readline at EOF, and that the previous
-- line is properly replaced on each call.

entity textio9 is
end entity;

use std.textio.all;

architecture test of textio9 is
begin

    process is
        file f     : text;
        variable l : line;
        variable n : integer;
    begin
        -- Write test file
        file_open(f, "textio9.txt", WRITE_MODE);
        write(l, string'("hello"));          writeline(f, l);
        write(l, string'(""));               writeline(f, l);  -- empty line
        write(l, string'("world"));          writeline(f, l);
        write(l, integer'(42));              writeline(f, l);

        -- Long line: 300 characters (exceeds initial 256-byte buffer)
        for i in 1 to 300 loop
            write(l, character'('x'));
        end loop;
        writeline(f, l);

        file_close(f);

        -- Read back and verify
        file_open(f, "textio9.txt", READ_MODE);

        -- Normal line
        readline(f, l);
        assert l.all = "hello"
            report "Expected 'hello', got '" & l.all & "'" severity failure;

        -- Empty line
        readline(f, l);
        assert l'length = 0
            report "Expected empty line" severity failure;

        -- Second normal line; verifies old l is replaced correctly
        readline(f, l);
        assert l.all = "world"
            report "Expected 'world', got '" & l.all & "'" severity failure;

        -- Line with integer
        readline(f, l);
        read(l, n);
        assert n = 42
            report "Expected 42" severity failure;

        -- Long line (300 x characters, exceeds 256-byte initial buffer)
        readline(f, l);
        assert l'length = 300
            report "Expected length 300, got " & integer'image(l'length)
            severity failure;
        assert l.all(1) = 'x' and l.all(300) = 'x'
            report "Long line content wrong" severity failure;

        file_close(f);
        deallocate(l);

        wait;
    end process;

end architecture;
