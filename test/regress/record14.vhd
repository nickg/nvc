entity sub is
    generic ( N : natural );
end entity;

architecture test of sub is
    type rec is record
        x : natural;
        y : bit_vector(1 to N);
        z : natural;
    end record;

    type rec2 is record
        x : natural;
        y : rec;
        z : natural;
    end record;

    function func (q : bit_vector) return natural is
        type rec3 is record
            x : natural;
            y : bit_vector(1 to q'length);
            z : natural;
        end record;
        variable r : rec3;
    begin
        r := ( 1, q, q'length );
        r.y(1) := '1';
        for i in r.y'range loop
            if i = 1 then
                assert r.y(i) = '1';
            else
                assert r.y(i) = '0';
            end if;
        end loop;
        return r.z;
    end function;

    signal s : rec;
    signal s2 : rec2;
begin

    p1: process is
        variable r : rec;
    begin
        assert r = (0, (1 to N => '0'), 0);
        r.y(1) := '1';
        assert r.y = (1 => '1', 2 to 4 => '0');

        assert func(r.y) = N;

        assert s = (0, (1 to N => '0'), 0);
        s.y(1) <= '1';
        wait for 1 ns;
        assert s.y = (1 => '1', 2 to 4 => '0');

        assert s2 = (0, (0, (1 to N => '0'), 0), 0);
        s2.y.y(1) <= '1';
        wait for 1 ns;
        assert s2.y.y = (1 => '1', 2 to 4 => '0');

        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity record14 is
end entity;

architecture test of record14 is
begin

    sub_i: entity work.sub generic map ( 4 );

end architecture;
