entity attr19 is
end entity ;

architecture arch of attr19 is

    type record_t is record
        a   :   integer ;
        b   :   bit ;
    end record ;

    function to_string(x : in record_t) return string is
    begin
        return "(a => " & integer'image(x.a) & ", b => " & bit'image(x.b) & ")" ;
    end function ;

    procedure monitor generic(type t ; function to_string(x : in t) return string is <>) parameter(x : in t) is
        procedure sub is
        begin
            std.textio.write(std.textio.output, monitor'instance_name & "/" & time'image(now) & ": " & to_string(x) & LF) ;
        end procedure;
    begin
        sub;
    end procedure ;

    procedure monitor is new monitor generic map(t => record_t) ;

    signal r : record_t ;

begin

    monitor(r) ;

    tb : process begin
        r.a <= 10 ;
        r.b <= '0' ;
        wait for 10 ns ;
        r.a <= 11 ;
        wait for 15 ns ;
        r.b <= '1' ;
        wait for 30 ns ;
        report "Done";
        std.env.stop ;
    end process ;

end architecture ;
