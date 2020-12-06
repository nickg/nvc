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

    process is
        variable x : bit;
    begin
        x := '1';
        wait for 1 ns;                  -- Prevent constant folding
        assert get_message(x) = "one";
        x := '0';
        wait for 1 ns;
        assert get_message(x) = "zero";
        wait;
    end process;

end architecture;
