package unreachable is
    function func (x : integer) return integer;
end package;

package body unreachable is
    function func (x : integer) return integer is
    begin
        if x > 0 then
            return x * 2;
        end if;
        -- Error here
    end function;
end package body;
