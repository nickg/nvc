library ieee;
use ieee.std_logic_1164.all;

package pack is
    function matching_cases (
        opcode: std_logic_vector
    ) return natural;

end package;

package body pack is

    function matching_cases (
        opcode: std_logic_vector
    ) return natural is

        variable bytes : natural := 0;

    begin
        case? opcode is
            when "00000000" =>
                bytes := 1;
            when "00000001" =>
                bytes := 1;
            when "010000--" =>
                bytes := 1;
            when "010000--" =>  -- SAME CONDITION AS ABOVE ARM!!!
                bytes := 2;
            when others =>
                bytes := 4;
        end case?;
        case ? opcode is
            when "--00--0000" =>
                bytes := 1;
            when "00--00-00-" =>
                bytes := 2;
        end case?;                      -- Error
        case ? opcode is
            when "--00--0000" =>
                bytes := 1;
            when "----------" =>
                bytes := 2;
        end case?;                      -- OK
        return bytes;
    end;

end package body;
