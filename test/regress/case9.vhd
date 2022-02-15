entity case9 is
end entity;

architecture test of case9 is
    type rec is record
        b : bit_vector(1 to 3);
    end record;

    signal x : rec;
    signal y : integer;
begin

    p1: process (x) is
    begin
        case x.b is
            when "100" => y <= 1;
            when "101" => y <= 2;
            when "111" => y <= 3;
            when others => y <= -1;
        end case;
    end process;

    p2: process is
    begin
        wait for 0 ns;
        assert y = -1;

        x <= ( b => "100" );
        wait for 1 ns;
        assert y = 1;

        x <= ( b => "101" );
        wait for 1 ns;
        assert y = 2;

        x <= ( b => "111" );
        wait for 1 ns;
        assert y = 3;

        wait;
    end process;

end architecture;
