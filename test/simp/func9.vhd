entity func9 is
end entity;

architecture test of func9 is

    constant msg0 : string := "zero";
    constant msg1 : string := "one";

    function get_message(x : in bit) return string is
    begin
        case x is
            when '0' => return msg0;
            when '1' => return msg1;
        end case;
    end function;

begin

    g1: if get_message('1') /= "one" generate
        a1: assert false;
    end generate;

    g2: if get_message('0') /= "zero" generate
        a1: assert false;
    end generate;

end architecture;
