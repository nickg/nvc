entity const3 is
end entity;

architecture test of const3 is

    type bit_str_map is array (bit) of string(1 to 4);

    constant const : bit_str_map := ( "zero", "one " );

begin

    process is
    begin
        report const('0');
        report const('1');
        wait;
    end process;

end architecture;
