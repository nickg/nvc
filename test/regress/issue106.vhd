-- sliced_array.vhdl
use std.textio.all;

package foo_const is
        constant foo1_c : integer := 0;
        constant foo2_c : integer := 7;
        constant addr1_c : bit_vector(foo2_c downto foo1_c) := x"02";
        constant addr2_c : bit_vector(foo2_c downto foo1_c) := x"03";
        constant addr3_c : bit_vector(foo2_c downto foo1_c) := x"04";
end foo_const;

use std.textio.all;

use work.foo_const.all;

entity SLICED_ARRAY is
        port(    IN1  : in  bit_vector(7 downto 0);
                 OUT1 : out bit_vector(7 downto 0) );
end SLICED_ARRAY;

architecture BEHAVIOUR of SLICED_ARRAY is
begin
        PR1:
        process( IN1 )
                variable l : line;
        begin
                case IN1 is
                        -- when addr1_c => write(l, string'("OK")); writeline(output, l); -- OK
                        when addr2_c(7 downto 0) => write(l, string'("OK addr2_c")); writeline(output, l); -- fails
                        when addr3_c(foo2_c downto foo1_c) => write(l, string'("OK addr3_c")); writeline(output, l); -- fails
                        when others => write(l, string'("other")); writeline(output, l);
                end case;
        end process;
end BEHAVIOUR;

-------------------------------------------------------------------------------
-- sliced_array_test.vhdl

use std.textio.all;

entity issue106 is
begin
end entity issue106 ;

architecture BEHAVIOUR of issue106 is
        component SLICED_ARRAY is
                port(    IN1  : in  bit_vector(7 downto 0);
                         OUT1 : out bit_vector(7 downto 0) );
        end component SLICED_ARRAY;

        signal S1 : bit_vector(7 downto 0);
        signal S2 : bit_vector(7 downto 0);
begin

        SLICED_ARRAY_INST : SLICED_ARRAY
                port map (    IN1  => S1,
                              OUT1 => S2 );
        process
                variable l : line;
        begin
            S1 <= x"10";
            wait for 1 ns;
            S1 <= x"03";
            wait for 1 ns;
            S1 <= x"04";
                wait;
        end process;


        other: process is
            constant c1 : bit_vector(7 downto 0) := "11110000";
            constant c2 : bit_vector(0 to 7) := "11001100";
            constant c3 : bit_vector(15 downto 8) := "11110000";
            variable v1, v2, v3 : bit_vector(3 downto 0);
        begin
            v1 := "1100";
            case v1 is
                when c1(5 downto 2) => report "ok1";
                when others => assert false;
            end case;
            v1 := "1111";
            case v1 is
                when c1(7 downto 4) => report "ok2";
                when others => assert false;
            end case;

            v2 := "1100";
            case v2 is
                when c2(4 to 7) => report "ok3";
                when others => assert false;
            end case;
            v2 := "0110";
            case v2 is
                when c2(3 to 6) => report "ok4";
                when others => assert false;
            end case;

            v3 := "1100";
            case v3 is
                when c3(13 downto 10) => report "ok5";
                when others => assert false;
            end case;
            v3 := "1111";
            case v3 is
                when c3(15 downto 12) => report "ok6";
                when others => assert false;
            end case;

            wait;
        end process;

end architecture BEHAVIOUR;
