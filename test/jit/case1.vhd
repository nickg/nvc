package case1 is
    type t is (a, b, c);
    function test1(x : t) return integer;
    function test2(x : bit_vector(1 to 4)) return integer;
end package;

package body case1 is

    function test1(x : t) return integer is
    begin
        case x is
            when a => return 10;
            when b => return 20;
            when c => return 30;
        end case;
    end function;

    function test2(x : bit_vector(1 to 4)) return integer is
        variable result : integer := 0;
    begin
        case x is
            when "0000" => result := 0;
            when "0001" => result := 1;
            when "0010" => result := 2;
            when "0011" => result := 3;
            when "0100" => result := 4;
            when "0101" => result := 5;
            when "0110" => result := 6;
            when "0111" => result := 7;
            when "1000" => result := 8;
            when "1001" => result := 9;
            when "1010" => result := 10;
            when "1011" => result := 11;
            when "1100" => result := 12;
            when "1101" => result := 13;
            when "1110" => result := 14;
            when "1111" => result := 15;
        end case;
        return result;
    end function;

end package body;
