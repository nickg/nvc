entity access11 is
end entity;

architecture test of access11 is
    type list;

    type list_ptr is access list;

    type list is record
        chain : list_ptr;
        item  : integer;
    end record;

    impure function make_list(item : integer) return list_ptr is
    begin
        return new list'(null, item);
    end function;

    shared variable l : list_ptr;
begin

    p1: process is
        variable tmp : list_ptr;
    begin
        for i in 1 to 10 loop
            tmp := l;
            l := make_list(i);
            l.chain := tmp;
            wait for 1 ns;
        end loop;
        wait;
    end process;

    p2: process is
        variable it : list_ptr;
        variable sum : integer := 0;
    begin
        wait for 1 hr;
        it := l;
        while it /= null loop
            sum := sum + it.all.item;
            it := it.chain;
        end loop;
        assert sum = 55;
        wait;
    end process;

end architecture;
