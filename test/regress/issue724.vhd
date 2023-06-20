use std.textio.all ;

entity issue724 is
end entity ;

architecture arch of issue724 is

    constant FNAME : string := "./dummy.txt" ;

begin

    tb : process
        file fin : text ;
        variable l : line ;
        variable a, b : integer ;
        variable count : natural := 0 ;
    begin
        file_open(fin, FNAME, write_mode) ;
        write(l, string'("hello, world"));
        writeline(fin, l);
        file_close(fin);

        file_open(fin, FNAME, read_mode) ;
        readline(fin, l) ;
        assert l.all /= "" report "Empty line read" severity failure ;
        file_close(fin) ;
        std.env.stop ;
    end process ;
end architecture;
