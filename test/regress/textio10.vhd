-- Test for native read(integer) and read(character) intrinsics

entity textio10 is
end entity;

use std.textio.all;

architecture test of textio10 is
begin

    process is
        file f     : text;
        variable l : line;
        variable n : integer;
        variable c : character;
        variable g : boolean;
    begin
        -- Write test file
        file_open(f, "textio10.txt", WRITE_MODE);
        write(l, string'("42"));           writeline(f, l);
        write(l, string'("  -7"));         writeline(f, l);
        write(l, string'("+100"));         writeline(f, l);
        write(l, string'("0"));            writeline(f, l);
        write(l, string'("  -  "));        writeline(f, l);
        -- dash without digit
        write(l, string'("abc"));          writeline(f, l);  -- non-numeric
        write(l, string'("X Y"));          writeline(f, l);  -- characters
        write(l, string'(""));             writeline(f, l);  -- empty line
        write(l, string'("99 Z"));         writeline(f, l);  -- int then char
        file_close(f);

        -- Read back and verify
        file_open(f, "textio10.txt", READ_MODE);

        -- read(integer, good): positive
        readline(f, l);
        read(l, n, g);
        assert g report "42: good expected" severity failure;
        assert n = 42
            report "Expected 42, got " & integer'image(n)
            severity failure;

        -- read(integer, good): negative with whitespace
        readline(f, l);
        read(l, n, g);
        assert g report "-7: good expected" severity failure;
        assert n = -7
            report "Expected -7, got " & integer'image(n)
            severity failure;

        -- read(integer): positive with sign (no good flag)
        readline(f, l);
        read(l, n);
        assert n = 100
            report "Expected 100, got " & integer'image(n)
            severity failure;

        -- read(integer, good): zero
        readline(f, l);
        read(l, n, g);
        assert g report "0: good expected" severity failure;
        assert n = 0
            report "Expected 0, got " & integer'image(n)
            severity failure;

        -- read(integer, good): dash without digit → not good
        readline(f, l);
        read(l, n, g);
        assert not g
            report "dash without digit: not good expected"
            severity failure;

        -- read(integer, good): non-numeric → not good
        readline(f, l);
        read(l, n, g);
        assert not g report "abc: not good expected" severity failure;

        -- read(character, good): X
        readline(f, l);
        read(l, c, g);
        assert g report "X: good expected" severity failure;
        assert c = 'X' report "Expected 'X'" severity failure;

        -- read(character): space after X
        read(l, c);
        assert c = ' ' report "Expected space" severity failure;

        -- read(character): Y
        read(l, c, g);
        assert g report "Y: good expected" severity failure;
        assert c = 'Y' report "Expected 'Y'" severity failure;

        -- read(character, good): end of line → not good
        read(l, c, g);
        assert not g report "EOL: not good expected" severity failure;

        -- empty line: read(character, good) → not good
        readline(f, l);
        read(l, c, g);
        assert not g report "empty line: not good expected" severity failure;

        -- read integer then character from same line
        readline(f, l);
        read(l, n, g);
        assert g report "99: good expected" severity failure;
        assert n = 99
            report "Expected 99, got " & integer'image(n)
            severity failure;
        read(l, c);    -- space
        assert c = ' ' report "Expected space after 99" severity failure;
        read(l, c);    -- Z
        assert c = 'Z'
            report "Expected Z, got '" & character'image(c) & "'"
            severity failure;

        file_close(f);
        deallocate(l);

        wait;
    end process;

end architecture;
