entity conv19 is
end entity;

architecture test of conv19 is
    type t_rec is record
        x : integer;
        y : bit;
    end record;

    function popcount (v : bit_vector) return natural is
        variable result : natural;
    begin
        for i in v'range loop
            if v(i) = '1' then
                result := result + 1;
            end if;
        end loop;
        return result;
    end function;

    signal s : bit_vector(1 to 3);
    signal t : bit;
begin

    b: block is
        port ( r : t_rec );
        port map (
            r.x => popcount(s),
            r.y => inertial t );
    begin

        check: process is
        begin
            assert r = (0, '0');
            wait for 0 ns;
            assert r = (2, '0');
            wait for 0 ns;
            assert r = (2, '1');
            wait for 1 ns;
            assert r = (1, '1');
            wait;
        end process;
    end block;

    stim: process is
    begin
        s <= "101";
        t <= '1';
        s <= transport "001" after 1 ns;
        wait;
    end process;

end architecture;
