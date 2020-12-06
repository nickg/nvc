package table is
    type STD_ULOGIC is ( 'U', 'X', '0', '1', 'Z', 'W', 'L', 'H', '-' );
    type STD_ULOGIC_VECTOR is array (NATURAL range <>) of STD_ULOGIC;
end package;

package body table is

    type stdlogic_table is array(STD_ULOGIC, STD_ULOGIC) of STD_ULOGIC;
    constant resolution_table : stdlogic_table := (
        ('U', 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U'),
        ('U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X'),
        ('U', 'X', '0', 'X', '0', '0', '0', '0', 'X'),
        ('U', 'X', 'X', '1', '1', '1', '1', '1', 'X'),
        ('U', 'X', '0', '1', 'Z', 'W', 'L', 'H', 'X'),
        ('U', 'X', '0', '1', 'W', 'W', 'W', 'W', 'X'),
        ('U', 'X', '0', '1', 'L', 'W', 'L', 'W', 'X'),
        ('U', 'X', '0', '1', 'H', 'W', 'W', 'H', 'X'),
        ('U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X') );

end package body;
