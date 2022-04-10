entity case10 is
end entity;

architecture test of case10 is
    type my_enum is (a, b, c);

    signal e : my_enum;
    signal i : natural;
begin

    update: process (i) is
    begin
        case i is
            when my_enum'pos(a) => e <= a;
            when my_enum'pos(b) => e <= b;
            when my_enum'pos(c) => e <= c;
            when others => null;
        end case;
    end process;

    main: process is
    begin
        i <= 0;
        wait for 1 ns;
        assert e = a;
        i <= 2;
        wait for 1 ns;
        assert e = c;
        wait;
    end process;

end architecture;
