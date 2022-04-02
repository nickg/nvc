entity record29 is
end entity;

architecture test of record29 is
    type rec is record
        x : bit_vector;
    end record;

    signal s : rec(x(1 to 3));
begin

    main: process is
    begin
        assert s.x = "000";
        assert s = (x => "000");
        s.x <= "101";
        wait for 1 ns;
        assert s.x = "101";
        assert s = (x => "101");
        s <= (x => "111");
        wait for 1 ns;
        assert s.x = "111";
        assert s = (x => "111");
        wait;
    end process;

end architecture;
