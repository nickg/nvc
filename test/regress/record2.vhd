entity record2 is
end entity;

architecture test of record2 is

    type rec is record
        x, y : integer;
    end record;

    procedure set_to(variable r : inout rec;
                     constant n : in integer) is
    begin
        r.x := n;
        r.y := r.x;
    end procedure;

begin

    process is
        variable r : rec;
    begin
        set_to(r, 5);
        assert r.x = 5;
        assert r.y = 5;
        wait;
    end process;

end architecture;
