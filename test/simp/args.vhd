entity args is
end entity;

architecture a of args is

    function func(x, y : in integer) return integer is
    begin
        return x + y;
    end function;

    procedure proc(x, y : in integer) is
    begin
    end procedure;

begin

    process is
        variable a, b, c : integer;
    begin
        c := func(x => a, y => b);
        c := func(a, y => b);
        c := func(y => b, x => a);
        proc(y => b, x => a);
    end process;

end architecture;
