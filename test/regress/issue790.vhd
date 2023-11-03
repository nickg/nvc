entity issue790 is
end entity;

architecture test of issue790 is
begin

    p1: process is
        variable s : string(1 to 100) := (others => 'x');
        variable e : natural := 0;
    begin
        s(1 to 5) := "12345";
        wait for 1 ns;
        while e < s'length and s(e + 1) /= 'x' loop
            e := e + 1;
        end loop;
        assert integer'value(s(1 to e)) = 12345;
        wait;
    end process;

end architecture;
