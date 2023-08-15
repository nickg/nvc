entity bitstr1 is
end entity;

architecture test of bitstr1 is
begin

    main: process is
    begin
        -- Examples from LRM
        assert B"1111_1111_1111" = string'("111111111111");
        assert X"FFF" = string'(b"1111_1111_1111");
        assert O"777" = string'(b"111_111_111");
        assert X"777" = string'(b"0111_0111_0111");
        assert B"XXXX_01LH" = string'("XXXX01LH");
        assert UO"27" = string'(b"010_111");
        assert UO"2C" = string'("010CCC");
        assert SX"3W" = string'("0011WWWW");
        assert D"35" = string'("100011");
        assert 12UB"X1" = string'("0000000000X1");
        assert 12SB"X1" = string'("XXXXXXXXXXX1");
        assert 12UX"F-" = string'("00001111----");
        assert 12SX"F-" = string'("11111111----");
        assert 12D"13" = string'(b"0000_0000_1101");
        assert 12UX"000WWW" = string'("WWWWWWWWWWWW");
        assert 12SX"FFFC00" = string'("110000000000");
        assert 12SX"XXXX00" = string'("XXXX00000000");

        -- Empty bit strings should be allowed
        assert 4x"" = string'("0000");
        assert d"" = string'("");
        wait;
    end process;

    p2: process is
        constant c1: STRING := B"1111_1111_1111";
        constant c2: BIT_VECTOR := X"FFF";
        type MVL is ('X', '0', '1', 'Z');
        type MVL_VECTOR is array (NATURAL range <>) of MVL;
        constant c3: MVL_VECTOR := O"777";
    begin
        assert c1'LENGTH = 12 and c2'LENGTH = 12 and c3 = "111111111";
        wait;
    end process;

end architecture;
