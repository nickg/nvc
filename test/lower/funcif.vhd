entity funcif is
end entity;

architecture test of funcif is

    function foo(x : integer) return integer is
    begin
        if x > 5 then
            return 1;
        else
            return 0;
        end if;
    end function;

begin

end architecture;
