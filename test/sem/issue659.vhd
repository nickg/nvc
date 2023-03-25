entity test is
end entity ;

architecture arch of test is

    procedure write_to_in(variable y : in integer) is
    begin
        y := y + 1 ;
    end procedure ;

begin
end architecture ;
