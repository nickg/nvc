entity concat1 is
end entity;

architecture test of concat1 is
    type int_array is array (integer range <>) of integer;
begin

    -- See LRM 93 section 9.2.5
    process is
        variable x : int_array(1 to 5);
        variable y : int_array(1 to 2);
        variable z : int_array(1 to 3);
        variable s : string(1 to 5);
    begin
        y := (1, 2);
        z := (3, 4, 5);
        x := y & z;
        assert x = (1, 2, 3, 4, 5);
        s := "ab" & "cde";
        assert s = "abcde";
        z := 0 & y;
        assert z = (0, 1, 2);
        z := y & 3;
        assert z = (1, 2, 3);
        y := 8 & 9;
        assert y = (8, 9);
        wait;
    end process;

end architecture;
