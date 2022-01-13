entity bounds15 is
end entity;

architecture test of bounds15 is

    function fact(n : natural) return positive is
    begin
        if n = 0 then
            return 1;
        else
            return n * fact(n - 1);
        end if;
    end function;

begin

    p1: process is
        variable x : integer;
    begin
        x := 5;
        report integer'image(fact(x));
        x := 0;
        report integer'image(fact(x));
        x := -1;
        report integer'image(fact(x));  -- Error
        wait;
    end process;

end architecture;
