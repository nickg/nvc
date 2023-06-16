package pack is
    type rec_t is record
        x : integer_vector;
    end record;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port ( p : in rec_t; o : out integer );
end entity;

architecture test of sub is
begin
    p1: process (p) is
        variable local_s : p'subtype;
        variable result : integer := 0;
    begin
        local_s := p;
        for i in local_s.x'reverse_range loop
            if local_s.x(i) > 0 then
                result := local_s.x(i);
                exit;
            end if;
        end loop;
        o <= result;
    end process;

end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity range2 is
end entity;

architecture test of range2 is
    signal s : rec_t(x(1 to 5)) := (x => (1, 2, 3, 0, 0));
    signal o : integer;
begin

    u: entity work.sub port map (s, o);

    check: process is
    begin
        wait for 1 ns;
        assert o = 3;
        s.x(4) <= 222;
        wait for 1 ns;
        assert o = 222;
        wait;
    end process;

end architecture;
