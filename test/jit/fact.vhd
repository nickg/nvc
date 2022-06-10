package pack is
    function fact(x : integer) return integer;
end package;

package body pack is
    function fact(x : integer) return integer is
        variable r : integer := 1;
    begin
        for i in 1 to x loop
            r := r * i;
        end loop;
        return r;
    end function;

    function fact_recur(x : integer) return integer is
    begin
        if x <= 1 then
            return 1;
        else
            return x * fact_recur(x - 1);
        end if;
    end function;

end package body;
