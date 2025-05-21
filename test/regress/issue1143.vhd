entity issue1143 is
end entity;

architecture test of issue1143 is
    signal x : character;

    impure function get_pos return integer is
    begin
        return character'pos(x);
    end function;
begin

    b: block is
        port (
            p1, p2 : in integer );
        port map (
            p1 => character'pos(x),
            p2 => get_pos);
    begin

        check: process is
        begin
            assert p1 = integer'left;
            --assert p2 = integer'left;
            wait for 0 ns;
            assert p1 = 0;
            --assert p2 = 0;
            wait for 0 ns;
            assert p1 = 97;
            --assert p2 = 97;
            wait for 1 ns;
            wait for 0 ns;
            assert p1 = 98;
            --assert p2 = 98;
            wait;
        end process;

    end block;

    x <= 'a', 'b' after 1 ns, 'c' after 2 ns;

end architecture;
