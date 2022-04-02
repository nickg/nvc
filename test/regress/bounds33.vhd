entity bounds33 is
end entity;

architecture test of bounds33 is
    type rec is record
        x : bit_vector;
    end record;

    procedure assign (p : out rec) is
    begin
        p := (x => "1111111111111111");
    end procedure;

begin

    main: process is
        variable s : rec(x(1 to 3));
    begin
        assert s.x = "000";
        assert s = (x => "000");
        s.x := "101";
        assert s.x = "101";
        assert s = (x => "101");
        assign(s);                      -- Error
        report to_string(s.x'length);
        assert s.x = "111";
        assert s = (x => "111");
        wait;
    end process;

end architecture;
