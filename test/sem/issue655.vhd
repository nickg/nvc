entity issue655 is
end entity ;

architecture casearch of issue655 is

    subtype too_high is integer range 500 to integer'high ;
    subtype too_low is integer range integer'low to -500;

begin

    process is
        variable x : integer;
    begin
        case x is
            when too_high => null ;
            when too_low  => null ;
            when others   => null ;
        end case ;
    end process ;

end architecture ;
