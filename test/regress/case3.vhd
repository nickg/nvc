entity case3 is
end entity;

architecture test of case3 is
    signal x       : bit_vector(3 downto 0);
    signal y, z, q : integer;
begin

    decode_y: with x select y <=
        0 when X"0",
        1 when X"1",
        2 when X"2",
        3 when X"3",
        4 when X"4",
        5 when X"5",
        6 when X"6",
        7 when X"7",
        8 when X"8",
        9 when X"9",
        10 when X"a",
        11 when X"b",
        12 when X"c",
        13 when X"d",
        14 when X"e",
        15 when X"f";

    decode_z: with x(3 downto 0) select z <=
        0 when X"0",
        1 when X"1",
        2 when X"2",
        3 when X"3",
        4 when X"4",
        5 when X"5",
        6 when X"6",
        7 when X"7",
        8 when X"8",
        9 when X"9",
        10 when X"a",
        11 when X"b",
        12 when X"c",
        13 when X"d",
        14 when X"e",
        15 when X"f";

    stim: process is
    begin
        wait for 0 ns;
        assert y = 0;
        assert z = 0;
        x <= X"4";
        wait for 1 ns;
        assert y = 4;
        assert y = 4;
        x <= X"f";
        wait for 1 ns;
        assert y = 15;
        assert z = 15;
        wait;
    end process;
    
end architecture;
