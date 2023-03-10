library ieee ;
use ieee.std_logic_1164.all ;

entity issue643 is
end entity;

architecture arch of issue643 is

    function check_case(x : std_logic_vector) return boolean is
    begin
        case? x is
            when "1--" => return true ;
            when "01-" => return true ;
            when "001" => return true ;
            when others => return false ;
        end case? ;
    end function;

begin

    tb: process is
    begin
        --assert check_case("1--") = true ; -- illegal but should be caught at runtime?
        assert check_case("110") = true ;
        assert check_case("100") = true ;
        assert check_case("101") = true ;
        assert check_case("000") = false ;
        wait;
    end process ;

end architecture ;
