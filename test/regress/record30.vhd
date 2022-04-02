entity record30 is
end entity;

architecture test of record30 is
    type rec is record
        x : bit_vector;
    end record;
begin

    main: process is
        variable s : rec(x(1 to 3));
    begin
        assert s.x = "000";
        assert s = (x => "000");
        s.x := "101";
        assert s.x = "101";
        assert s = (x => "101");
        s := (x => "111");
        assert s.x = "111";
        assert s = (x => "111");
        wait;
    end process;

end architecture;
