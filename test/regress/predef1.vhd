entity predef1 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of predef1 is
    type my_int is range 1 to 10;
    type my_enum is (X, Y, FOO, BAR);
    type int_vector is array (natural range <>) of integer;
    type my_char is ('a', 'b', 'c');
    type char_vector is array (natural range <>) of my_char;
    type my_phs is range 0 to 1000
        units
            u_foo;
            u_bar = 10 u_foo;
        end units;

    signal sa : bit := '0';
    signal sb : boolean := true;
    signal sl : std_logic := '0';
    signal sv : bit_vector(1 to 3);
    signal sx : std_logic_vector(1 to 3);
begin

    main: process is
        variable b : boolean;
        variable i : integer;
        variable m : my_int;
        variable e : my_enum;
        variable r : real := 1.23456;
        variable v : int_vector(1 to 3) := (1, 2, 3);
        variable c : char_vector(1 to 3) := "bca";
    begin
        -----------------------------------------------------------------------
        -- MINIMUM

        assert minimum(1, 2) = 1;
        m := 5;
        b := true;
        wait for 1 ns;
        assert minimum(4, m) = 4;
        assert minimum(b, false) = false;
        assert minimum(int_vector'(5, -1, 2)) = -1;
        assert minimum(v) = 1;

        -----------------------------------------------------------------------
        -- MAXIMUM

        assert maximum(2, 1) = 2;
        m := 6;
        wait for 1 ns;
        assert maximum(4, m) = 6;
        assert maximum(b, false) = true;
        assert maximum(int_vector'(5, -1, 2)) = 5;
        assert maximum(v) = 3;
        assert maximum(1 ns, 1 hr) = 1 hr;

        -----------------------------------------------------------------------
        -- TO_STRING

        assert to_string(my_int'(7)) = "7";
        assert to_string(m) = "6";
        assert to_string(foo) = "foo";
        assert to_string(e) = my_enum'image(e);

        report to_string(now);
        assert to_string(now) = "2000000 fs";
        report to_string(now, ns);
        assert to_string(now, ns) = "2 ns";
        assert to_string(50 ns, unit => us) = "0.05 us";
        assert to_string(value => 1 min, unit => hr) = "0.0166666666666667 hr";
        assert to_string(1310408750 ps, unit => ns) = "1310408.75 ns";

        assert to_string(r)(1 to 7) = "1.23456";
        report to_string(r, 2);
        assert to_string(r, 2) = "1.23";
        report to_string(r, 0);
        assert to_string(r, 0)(1 to 7) = "1.23456";

        report to_string(r, "%1.1f");
        assert to_string(r, "%1.1f") = "1.2";

        r := 0.0000000005;
        report to_string(value => r, digits => 9);

        report to_string(char_vector'("abc"));
        assert to_string(char_vector'("abc")) = "abc";
        assert to_string(c) = "bca";
        assert to_string(bit_vector'("110")) = "110";
        assert to_string(bit_vector'("110")) & "..." = "110...";
        assert to_string(1 ns) = "1000000 fs";
        assert to_string(1 u_bar) = "10 u_foo";
        assert to_string(bit'( '1' )) = "1";
        assert to_string(character'( 'X' )) = "X";

        -----------------------------------------------------------------------
        -- TO_BSTRING

        assert to_bstring(bit_vector'("101")) = "101";

        -----------------------------------------------------------------------
        -- TO_HSTRING

        report to_hstring(bit_vector'("10101111"));
        assert to_hstring(bit_vector'("10101111")) = "AF";
        assert to_hstring(bit_vector'("01100")) = "0C";
        assert to_hex_string(bit_vector'("111")) = "7";

        -----------------------------------------------------------------------
        -- TO_OSTRING

        report to_ostring(bit_vector'("10101111"));
        assert to_ostring(bit_vector'("10101111")) = "257";
        assert to_ostring(bit_vector'("01100")) = "14";
        assert to_octal_string(bit_vector'("0")) = "0";

        -----------------------------------------------------------------------
        -- RISING_EDGE / FALLING_EDGE

        sa <= '1';
        sb <= false;
        wait for 0 ns;
        assert sa'event and sa = '1';
        assert rising_edge(sa);
        assert not falling_edge(sa);
        assert sb'event and sb = false;
        assert not rising_edge(sb);
        assert falling_edge(sb);

        -----------------------------------------------------------------------
        -- Matching comparison

        sa <= '1';
        wait for 0 ns;
        assert (sa ?= '1') = '1';
        assert (sa ?/= '0') = '1';
        assert (sa ?< '1') = '0';
        assert (sa ?<= '1') = '1';
        assert (sa ?> '0') = '1';
        assert (sa ?>= '1') = '1';

        sl <= '1';
        wait for 0 ns;
        assert (sl ?= '1') = '1';
        assert (sl ?/= '0') = '1';
        assert (sl ?< '1') = '0';
        assert (sl ?<= '1') = '1';
        assert (sl ?> '0') = '1';
        assert (sl ?>= '1') = '1';
        assert (sl ?= 'Z') = 'X';
        assert (sl ?< 'H') = '0';
        assert ('0' ?<= sl) = '1';

        sv <= "111";
        wait for 0 ns;
        assert (sv ?= "111") = '1';
        assert (sv ?= "011") = '0';
        assert (sv ?/= "000") = '1';

        -----------------------------------------------------------------------
        -- Mixed array/scalar bit_vector operations

        sa <= '1';
        sv <= "101";
        wait for 1 ns;
        assert (sa and sv) = "101";
        assert (sa or sv) = "111";
        assert (sv xor sa) = "010";
        assert (sv nor sa) = "000";
        assert (sv nand sa) = "010";
        assert (sa xnor sv) = "101";

        wait;
    end process;

end architecture;
