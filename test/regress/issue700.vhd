use std.textio.all ;

entity issue700 is
end entity ;

architecture arch of issue700 is

begin

    tb : process
        variable s : string(1 to 6) ;
    begin
        s(3 to 4) := "Hi" ;
        s(5) := SOH;
        s(6) := DEL;
        report s ;
        write(output, s) ;
        wait;
    end process ;

end architecture ;
