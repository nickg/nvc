entity access1 is
end entity;

architecture test of access1 is
    type int_ptr is access integer;

    type list;

    type list_ptr is access list;

    type list is record
        link  : list_ptr;
        value : integer;
    end record;

    function list_add(l : list_ptr; v : integer) return list_ptr is
        variable n : list_ptr;
    begin
        n := new list;
        n.link  := l;
        n.value := v;
        return n;
    end function;

    procedure list_print(l : in list_ptr) is
    begin
        if l /= null then
            report integer'image(l.all.value);
            list_print(l.all.link);
        end if;
    end procedure;

    signal p1_done : boolean := false;

begin

    p1: process is
        variable p, q : int_ptr;
    begin
        assert p = null;
        p := new integer;
        p.all := 5;
        assert p.all = 5;
        q := p;
        assert q.all = 5;
        q.all := 6;
        assert p.all = 6;
        deallocate(p);
        --assert p = null;
        p1_done <= true;
        wait;
    end process;

    p2: process is
        variable l, p : list_ptr;
    begin
        wait until p1_done;

        for i in 1 to 10 loop
            l := list_add(l, i);
        end loop;

        list_print(l);
        --list_free(l);

        wait;
    end process;

end architecture;
