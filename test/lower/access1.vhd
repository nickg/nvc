entity access1 is
end entity;

architecture test of access1 is
    type list;

    type list_ptr is access list;

    type list is record
        link  : list_ptr;
        value : integer;
    end record;

    procedure list_add(l : inout list_ptr; v : integer) is
        variable n : list_ptr;
    begin
        n := new list;
        n.link  := l;
        n.value := v;
        l := n;
    end procedure;
begin

end architecture;
