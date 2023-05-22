entity ieee11 is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture test of ieee11 is
    signal s1 : std_logic_vector(16 downto 0) := "U01HLX-ZW101010X1";
    signal s2 : unsigned(16 downto 0) := "U01HLX-ZW101010X1";
    signal s3 : unsigned(31 downto 0) := X"12345678";
    signal s4 : unsigned(0 downto 1);
    signal s5 : signed(15 downto 0) := to_signed(-1000, 16);
    signal s6 : signed(0 downto 1);
    signal s7 : unsigned(7 downto 0) := X"10";
    signal s8 : unsigned(16 downto 0) := '0' & X"0010";

    function is_null (x : unsigned) return boolean is
    begin
        return x'length = 0;
    end function;

    function is_null (x : signed) return boolean is
    begin
        return x'length = 0;
    end function;

begin

    tb: process is
    begin
        assert to_x01(s1) = "X0110XXXX101010X1";
        assert to_01(s2, '0') = (16 downto 0 => '0');
        assert to_01(s3, '0') = X"12345678";
        assert is_null(to_01(s4));
        assert to_01(unsigned'("LLLLLLLLL"), '0') = "0000";
        assert resize(s3, 40) = X"0012345678";
        assert resize(s3, 32) = X"12345678";
        assert resize(s3, 8) = X"78";
        assert is_null(resize(s3, 0));
        assert resize(s5, 8) = "10011000";
        assert resize(s5, 20) = "11111111110000011000";
        assert is_null(resize(s5, 0));
        assert resize(s6, 8) = X"00";
        assert s7 + 5 = X"15";
        assert s7 + X"001" = X"011";
        assert s7 + X"f0" = X"00";
        assert s7 + X"22" = X"32";
        assert s8 + X"ffff" = "10000000000001111";
        assert (X"0000" + to_signed(-1, 4)) = X"ffff";
        wait;
    end process;

end architecture;
