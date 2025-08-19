entity sub is
    generic (TKEEP_WIDTH_C : integer);
end entity;

architecture test of sub is
    function TKEEP_REDUCE(tkeep : bit_vector(TKEEP_WIDTH_C-1 downto 0);
                          tkeep_width : integer) return bit_vector is
    begin
        case tkeep_width is
            when 16 =>
                case tkeep is
                    when x"0001" => return "0000";
                    when x"0003" => return "0001";
                    when x"0007" => return "0010";
                    when x"000F" => return "0011";
                    when x"001F" => return "0100";
                    when x"003F" => return "0101";
                    when x"007F" => return "0110";
                    when x"00FF" => return "0111";
                    when x"01FF" => return "1000";
                    when x"03FF" => return "1001";
                    when x"07FF" => return "1010";
                    when x"0FFF" => return "1011";
                    when x"1FFF" => return "1100";
                    when x"3FFF" => return "1101";
                    when x"7FFF" => return "1110";
                    when x"FFFF" => return "1111";
                    when others => return "0000"; -- bad value
                end case;
            when 8 =>
                case tkeep is
                    when x"01" => return "000";
                    when x"03" => return "001";
                    when x"07" => return "010";
                    when x"0F" => return "011";
                    when x"1F" => return "100";
                    when x"3F" => return "101";
                    when x"7F" => return "110";
                    when x"FF" => return "111";
                    when others => return "000"; -- bad value
                end case;
            when 4 =>
                case tkeep is
                    when x"1" => return "00";
                    when x"3" => return "01";
                    when x"7" => return "10";
                    when x"F" => return "11";
                    when others => return "00"; -- bad value
                end case;
            when 2 =>
                case tkeep is
                    when "01" => return "0";
                    when "11" => return "1";
                    when others => return "0"; -- bad value
                end case;
            when others =>
                return tkeep;
        end case;
    end TKEEP_REDUCE;
begin

    process is
    begin
        assert tkeep_reduce(x"00FF", 16) = "0111";
        assert tkeep_reduce(x"0003", 8) = "000";
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity issue1267 is
end entity;

architecture test of issue1267 is
begin

    u: entity work.sub
        generic map (TKEEP_WIDTH_C => 16);

end architecture;
