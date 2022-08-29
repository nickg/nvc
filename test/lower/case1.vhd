entity case7 is
end entity;

architecture test of case7 is
    constant C1 : bit_vector(3 downto 0) := X"1";
    constant C2 : bit_vector(3 downto 0) := X"2";
    alias C3 : bit_vector(3 downto 0) is C1;

    signal x : bit_vector(7 downto 0);
    signal y : integer;
begin

    testp: process (x) is
    begin
        case x is
            when C1 & X"0" =>
                y <= 5;
            when C1 & X"8" =>
                y <= 6;
            when C2 & X"2" =>
                y <= 10;
            when C3 & X"3" =>
                y <= 5;
            when others =>
                y <= 0;
        end case;
    end process;

end architecture;
