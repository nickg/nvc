library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
-- use ieee.numeric_std.all;
-- use ieee.std_logic_unsigned.all;

entity issue352 is
end entity;

architecture arch of issue352 is
    signal FixRealKCM_F400_uid2_Rtemp :  std_logic_vector(4 downto 0) := "11111";
    signal R  :std_logic_vector(5 downto 0);

    function check_dims(x : unsigned) return unsigned is
        alias ax : unsigned(1 to x'length) is x;
        variable s : string(1 to x'length);
    begin
        assert x'low >= unsigned'low;
        assert x'high <= unsigned'high;

        for i in 1 to x'length loop
            s(i) := std_logic'image(ax(i))(2);
        end loop;
        report s;

        return x;
    end function;

begin
   -- R <= "000000" - (FixRealKCM_F400_uid2_Rtemp(4) & FixRealKCM_F400_uid2_Rtemp(4 downto 0));
    R <= std_logic_vector(unsigned'(unsigned'("00000") - check_dims(unsigned (FixRealKCM_F400_uid2_Rtemp(4) & FixRealKCM_F400_uid2_Rtemp(4 downto 0)))));

    process is
    begin
        wait for 1 ns;
        assert R = "000001";
        assert FixRealKCM_F400_uid2_Rtemp'last_event = time'high;
        wait;
    end process;
end architecture;
