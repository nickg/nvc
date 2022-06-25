-- Test case from Brian Padalino
--
entity line_issue is end entity ;

architecture arch of line_issue is
    type line is access string;
begin

    tb : process
        variable fmt : line     := new string'("{>8f}") ;
        constant s   : string   := "abcd" ;
        -- Note length should be an attribute 'length
        constant len : natural  := fmt.length ;
        constant slen : natural := s'length ;
    begin
        report "fmt len: " & integer'image(len) ;
        report "s   len: " & integer'image(slen) ;
    end process ;

end architecture ;
