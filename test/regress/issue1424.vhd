entity issue1424 is
end entity ;

architecture arch of issue1424 is

    type bit_matrix is array(1 to 3) of bit_vector(1 to 5) ;

    procedure print(x : string) is
        use std.textio.line ;
        use std.textio.output ;
        use std.textio.write ;
        use std.textio.writeline ;
        variable l : line ;
    begin
        std.textio.write(l, x) ;
        std.textio.writeline(output, l) ;
    end procedure ;

begin

    tb : process
        variable msg : bit_matrix := (1 to 2 => (others => '1'), others => (others => '0')) ;
        constant s : string := to_string(msg) ;
        variable l : std.textio.line ;
    begin
        print("String length: " & integer'image(s'length) & " but should be at least " & integer'image(msg'length * msg'element'length)) ;
        print("Array") ;
        print(s) ;
        print("Individual Elements") ;
        for idx in msg'range loop
            print(to_string(msg(idx))) ;
        end loop ;
        std.env.stop ;
    end process ;

end architecture ;
