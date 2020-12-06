package constarr is
    function bit_to_char (b : bit) return character;
end package;

package body constarr is

    function bit_to_char (b : bit) return character is
        type table_t is array (bit) of character;
        constant table : table_t := ( '0' => '0',
                                      '1' => '1' );  -- OK
    begin
        return table(b);
    end function;

    constant c1 : character := bit_to_char('1');
    constant c2 : character := bit_to_char('0');

end package body;
