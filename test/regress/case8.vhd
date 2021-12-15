entity case8 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of case8 is

    function toint32(b : std_logic_vector(31 downto 0)) return integer is
    begin
        -- This uses a non-exact case map as 4*32 > 64
        case b is
            when X"00000000" => return 0;
            when X"00000100" => return 1;
            when X"00000101" => return 2;
            when X"00000011" => return 3;
            when X"00001001" => return 4;
            when X"00004141" => return 5;
            when X"00002521" => return 6;
            when X"10005211" => return 7;
            when X"ffff0001" => return 8;
            when X"ffff1000" => return 9;
            when X"ffff52af" => return 10;
            when X"ffffffff" => return 11;
            when X"ffffabcd" => return 12;
            when X"ffffabed" => return 13;
            when X"ffff1415" => return 14;
            when X"ffff5252" => return 15;

            -- These two have hash collisions
            when "LHH-HWUL-LHZ0UHH01WXXWXUUZX-WXUU" => return 555;
            when "0X0L0WWUHLX0Z1Z-L---L-LXZ0UHZ-0Z" => return 666;
            when others => return -1;
        end case;
    end function;

begin

    process is
        variable b : std_logic_vector(31 downto 0);
    begin
        assert toint32(X"00000000") = 0;
        b := X"00004141"; assert toint32(b) = 5;
        b := X"00001001"; assert toint32(b) = 4;
        b := X"00000101"; assert toint32(b) = 2;
        b := X"abab1101"; assert toint32(b) = -1;
        b := X"ffff52af"; assert toint32(b) = 10;
        b := X"ffff52ae"; assert toint32(b) = -1;

        -- The following three cases all hash to the same value

        b := "LHH-HWUL-LHZ0UHH01WXXWXUUZX-WXUU";
        assert toint32(b) = 555;

        b := "0X0L0WWUHLX0Z1Z-L---L-LXZ0UHZ-0Z";
        assert toint32(b) = 666;

        b := "Z-LXHW0XH-0W1111ZXWW1XLLZULX-HU1";
        assert toint32(b) = -1;

        wait;
    end process;

end architecture;
