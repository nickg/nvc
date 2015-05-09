entity case6 is
end entity;

architecture test of case6 is
    signal x, y : integer;
begin

    process (x) is
    begin
        case x is
            when 1 to 5 =>
                y <= 1;
            when 6 =>
                y <= 2;
            when 7 to 10 =>
                y <= 3;
            when others =>
                y <= 4;
        end case;
    end process;

    process is
    begin
        wait for 1 ns;
        assert y = 4;
        x <= 2;
        wait for 1 ns;
        assert y = 1;
        x <= 10;
        wait for 1 ns;
        assert y = 3;
        wait;
    end process;

end architecture;
