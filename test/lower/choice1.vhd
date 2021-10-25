entity choice1 is
end entity;

architecture test of choice1 is
    signal s : integer;
begin

    p1: process is
        variable x : integer;
    begin
        case s is
            when 1 | 2 =>
                x := 3;
            when 3 | 4 | 5 =>
                x := 4;
            when integer'low to 0 =>
                x := -1;
            when others =>
                x := 5;
        end case;
        wait;
    end process;

end architecture;
