entity issue862 is
end entity ;

architecture arch of issue862 is

    type bv_2d is array(integer range <>, integer range <>) of bit_vector ;

    constant ROWS : positive := 1 ;
    constant COLS : positive := 2 ;

    -- Assume first index is rows, next index is columns
    signal a : bv_2d(0 to 10, 0 to 10)(9 downto 0) ;

    procedure print(x : in string) is
        variable l : std.textio.line ;
    begin
        std.textio.write(l, x) ;
        std.textio.writeline(std.textio.output, l) ;
    end procedure ;

    procedure print(name : in string ; x : in bv_2d) is
    begin
        print(
            name & " is matrix " & integer'image(x'length(ROWS)) & " rows and " &
            integer'image(x'length(COLS)) & " with elements of length " & integer'image(x'element'length)
        ) ;
    end procedure ;

begin

    tb : process
    begin
        print("a", a) ;
        std.env.stop ;
    end process ;

end architecture ;
