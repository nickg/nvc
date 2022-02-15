entity sub is
end entity;

architecture test of sub is

    type rec is record
        x : integer;
    end record;

    constant c : rec := (x => 2);

    signal ss : rec := c;

    function add1(x : integer) return integer is
    begin
        return x + 1;
    end function;

begin

    process is
        variable r : rec := c;
    begin
        r.x := add1(ss.x);
        assert r.x = 3;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity record9 is
end entity;

architecture test of record9 is

    type rec is record
        x : bit_vector(1 to 3);
    end record;

    constant c : rec := (x => "101");

    signal s : rec := c;

begin

    uut: entity work.sub;

    s.x <= "111";

    process is
    begin
        assert s = c;
        wait for 1 ns;
        assert s = (x => "111");
        wait;
    end process;

end architecture;
