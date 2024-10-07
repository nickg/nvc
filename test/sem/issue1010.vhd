package pack is
    function get_abs_ext return integer;
end package;

package body pack is
    function get_abs_ext return integer is
    begin
        return <<signal .ename_bug.cnt : integer >>;  -- Error
    end;
end package body;
