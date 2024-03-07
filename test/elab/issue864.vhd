entity issue864 is
end entity ;

architecture arch of issue864 is

    impure function from_file(fname : in string) return real is
        use std.textio.all ;
        file f     : text ;
        variable s : file_open_status ;
        variable l : line ;
        variable r : real ;
    begin
        file_open(s, f, fname, READ_MODE) ;
        if s /= OPEN_OK then
            report "Could not open: " & fname severity failure ;
        end if ;
        readline(f, l) ;
        read(l, r) ;
        return r ;
    end function ;

    constant r : real := from_file("./nonexistent.txt") ;

begin
end architecture ;
