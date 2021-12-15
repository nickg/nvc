entity case2 is
end entity;

architecture test of case2 is

    function toint4(b : bit_vector(3 downto 0)) return integer is
    begin
        case b is
            when X"0" => return 0;
            when X"1" => return 1;
            when X"2" => return 2;
            when X"3" => return 3;
            when X"4" => return 4;
            when X"5" => return 5;
            when X"6" => return 6;
            when X"7" => return 7;
            when X"8" => return 8;
            when X"9" => return 9;
            when X"a" => return 10;
            when X"b" => return 11;
            when X"c" => return 12;
            when X"d" => return 13;
            when X"e" => return 14;
            when X"f" => return 15;
        end case;
    end function;

    function toint3(b : bit_vector(3 downto 0)) return integer is
    begin
        case b is
            when X"0" => return 0;
            when X"1" => return 1;
            when X"2" => return 2;
            when X"3" => return 3;
            when X"4" => return 4;
            when X"5" => return 5;
            when X"6" => return 6;
            when X"7" => return 7;
            when others => return -1;
        end case;
    end function;

begin

    process is
        variable b : bit_vector(3 downto 0);
    begin
        assert toint4(X"4") = 4;
        b := X"a";
        assert toint4(b) = 10;
        assert toint4(X"f") = 15;
        assert toint3(X"5") = 5;
        assert toint3(X"c") = -1;
        wait;
    end process;

end architecture;
