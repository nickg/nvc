entity case3 is
end entity;

architecture test of case3 is
    signal s : bit_vector(1 to 3);
    signal t : bit;
begin

    p1: process (s) is
    begin
        case s is
            when "100" =>
                t <= '1';
            when "101" =>
                t <= '0';
            when "100" =>               -- Error
                t <= '1';
            when others =>
                null;
        end case;
    end process;

end architecture;
