entity test is
end entity ;

architecture arch of test is

    type real_matrix is array (natural range <>, natural range <>) of real ;
    constant m : real_matrix := ( (1.0, 2.0, 3.0, 4.0), (2.0, 3.0, 4.0, 5.0) ) ;

    procedure print(x : in string) is
    begin
    end procedure ;

begin

    tb : process
    begin
        print("m length: " & integer'image(m'length(0))) ;  -- Error
        wait;
    end process ;

end architecture ;
