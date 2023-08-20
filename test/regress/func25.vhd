entity func25 is
end entity;

architecture test of func25 is

    function change_bounds (s : string; l, r : positive) return string is
        alias ss : string(l to r) is s;
    begin
        return ss;
    end function;

    impure function get_string (c : character) return string is
        variable s : string(1 to 15);
    begin
        s := "hello, world! " & c;
        return change_bounds(s, 101, 115);  -- Returns a pointer to S
    end function;

    function get_left (s : string) return positive is
    begin
        return s'left;
    end function;

    signal c : character := 'x';

begin

    p1: process is
    begin
        wait for 1 ns;
        report get_string(c);
        assert get_string(c) = "hello, world! x";
        assert get_left(get_string(c)) = 101;
        wait;
    end process;

end architecture;
