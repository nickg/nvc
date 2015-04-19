package index is
    subtype myRange is natural range 7 downto 0;
    constant my_range: bit_vector (myRange) := (others => '0');
    function "+" (l: bit_vector; stride: natural) return bit_vector;
end package;

package body index is

    function "+" (l: bit_vector;
                  stride: natural) return bit_vector is
        variable retval: bit_vector(
                        l'LEFT  + l'LENGTH * stride
                        downto
                        l'RIGHT + l'LENGTH * stride );
    begin
        -- report "stride = " & INTEGER'IMAGE(stride);
        -- report "retval'LEFT  = " & INTEGER'IMAGE(retval'LEFT);
        -- report "retval'RIGHT = " & INTEGER'IMAGE(retval'RIGHT);
        return retval;
    end function;

end package body;

-------------------------------------------------------------------------------

use work.index.all;

entity range1 is
end entity;

architecture foo of range1 is
    signal big_range: bit_vector (63 downto 0) := x"feedfacedeadbeef";
    signal little_range: bit_vector (myRange);

    function to_string(inp: bit_vector) return string is
        variable image_str: string (1 to inp'length);
        alias input_str:  bit_vector (1 to inp'length) is inp;
    begin
        for i in input_str'range loop
            image_str(i) := character'VALUE(bit'IMAGE(input_str(i)));
        end loop;
        -- report "image_str = " & image_str;
        return image_str;
    end;

begin
STIMULUS:
    process
    begin
        wait for 1 ns;
        for stride in myRange loop
            report "stride = " & INTEGER'IMAGE(stride);
            little_range <= big_range( "+"(my_range, stride)'RANGE );
            wait for 1 ns;
        end loop;
        wait;
    end process;
MONITOR:
    process (little_range)
    begin
        report "little_range = " & to_string (little_range);
    end process;
end architecture;
