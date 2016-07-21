entity case7 is
end entity;

architecture test of case7 is
    constant C1 : bit_vector(3 downto 0) := X"1";
    constant C2 : bit_vector(3 downto 0) := X"2";

    signal x : bit_vector(7 downto 0);
    signal y : integer;
begin

    process (x) is
    begin
        case x is
            when C1 & X"0" =>
                y <= 5;
            when C1 & X"8" =>
                y <= 6;
            when C2 & X"0" =>
                y <= 10;
            when others =>
                y <= 0;
        end case;
    end process;

    process is
    begin
        x <= X"10";
        wait for 1 ns;
        assert y = 5;

        x <= X"18";
        wait for 1 ns;
        assert y = 6;

        x <= X"20";
        wait for 1 ns;
        assert y = 10;

        x <= X"21";
        wait for 1 ns;
        assert y = 0;

        wait;
    end process;

end architecture;
