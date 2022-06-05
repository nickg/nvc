package pack is

    type rec is record
        x : integer;
    end record;

    type pt is protected
        impure function next_id return integer;
    end protected;

    impure function get_rec return rec;

    function get_id (r : rec) return integer;

end package;

package body pack is

    type pt is protected body
        variable ctr : integer := 0;

        impure function next_id return integer is
        begin
            ctr := ctr + 1;
            return ctr;
        end function;
    end protected body;

    shared variable p : pt;

    impure function get_rec return rec is
    begin
        return (id => pt.next_id);      -- Error
    end function;

    function get_id (r : rec) return integer is
    begin
        return r.x;
    end function;
end package body;
