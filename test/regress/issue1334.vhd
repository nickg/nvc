entity issue1334 is
end entity;

architecture test of issue1334 is
    function resolved_max (x : integer_vector) return integer is
        variable r : integer;
    begin
        for i in x'range loop
            r := maximum(r, x(i));
        end loop;
        return r;
    end function;

    type t_rec is record
        f : resolved_max integer;
    end record;

    signal s : t_rec;
begin

    s <= (f => 7);

    process is
    begin
        s.f <= 1;
        wait for 0 ns;
        assert s.f = 7;
        s.f <= 100;
        wait for 0 ns;
        assert s.f = 100;
        wait;
    end process;

end architecture;
