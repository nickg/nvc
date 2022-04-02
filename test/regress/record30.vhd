entity record30 is
end entity;

architecture test of record30 is
    type int_ptr is access integer;

    type pair is record
        x, y : natural;
    end record;

    type rec is record
        x : bit_vector;
        y : int_ptr;
        z : pair;
    end record;
begin

    main: process is
        variable s : rec(x(1 to 3));
    begin
        assert s.x = "000";
        assert s = (x => "000", y => null, z => (0, 0));
        s.x := "101";
        assert s.x = "101";
        assert s = (x => "101", y => null, z => (0, 0));
        s := (x => "111", y => null, z => (0, 0));
        assert s.x = "111";
        assert s = (x => "111", y => null, z => (0, 0));
        wait;
    end process;

end architecture;
