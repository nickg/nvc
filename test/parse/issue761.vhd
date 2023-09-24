entity issue761 is
end entity;

architecture test of issue761 is

    procedure test1 (id : natural; arg1, arg2 : integer; msg : string) is
    begin
    end procedure;
    procedure test1 (id : natural; arg1, arg2 : real; msg : string) is
    begin
    end procedure;

    function get (x : string; i : integer) return integer is
    begin
        return 0;
    end function;
    function get (x : string; i : integer) return real is
    begin
        return 0.0;
    end function;

begin

    p1: process is
        constant param : string := "foo";
    begin
        for i in 1 to 5 loop
            test1(5, get(param, i), -i, "blah");  -- OK
        end loop;
        wait;
    end process;

end architecture;
