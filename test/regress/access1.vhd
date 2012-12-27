entity access1 is
end entity;

architecture test of access1 is
    type int_ptr is access integer;
begin

    process is
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
        wait;
    end process;

end architecture;
