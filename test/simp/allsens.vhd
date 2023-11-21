entity allsens is
end entity;

architecture test of allsens is
    type bv_array is array (natural range <>) of bit_vector(1 to 3);
    signal x, y : bit;
    signal v    : bit_vector(1 to 3);
    signal z    : bv_array(1 to 3);
    signal n    : integer range v'range := 3;
    constant c  : bit_vector(1 to 3) := "101";
begin

    p0: process (all) is                -- (y)
    begin
        x <= y;
    end process;

    p1: assert v(1) = '0';              -- v(1)

    p2: assert v(n) = '0';              -- v, n

    p3: assert z(1)(2) = '0';           -- z(1)(2)

    p4: assert z(n)(2) = '0';           -- z, n

    p5: assert z(2)(n) = '0';           -- z(2), n

    p6: assert v(1 to 2) = "00";        -- v(1 to 2)

    p7: assert v(1 to n) = "00";        -- v, n

    p8: assert z(n)(1 to 2) = "00";     -- z, n

    p9: assert z(1 to 2)(n) = "000";    -- z(1 to 2), n

    p10: process (all) is               -- n, y
    begin
        v(n) <= y;
    end process;

    p11: process (all) is               -- n, y, v(1), z(1)
        variable b : bit_vector(0 to 1);
    begin
        if n > 5 then
            x <= y;
        else
            b(bit'pos(v(1))) := z(1)(n);
        end if;
    end process;

    p12: process (all) is               -- x, v(1), v(2)
        procedure proc (signal s : out bit) is
        begin
            s <= v(2);
        end procedure;
    begin
        case x is
            when '0' =>
                v(2) <= v(1);
            when '1' =>
                proc(y);
        end case;
    end process;

    p13: process (all) is               -- n, v
    begin
        for i in 1 to n loop
            assert v(i) = '1';
        end loop;
        while n < 5 loop
            n <= n + 1;
        end loop;
    end process;

    p14: v <= x & y & v(2);             -- x, y, v(2)

    p15: x <= c(n);                     -- n

end architecture;
