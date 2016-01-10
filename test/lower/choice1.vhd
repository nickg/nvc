entity choice1 is
end entity;

architecture test of choice1 is
    signal s : integer;
begin

    process is
        variable x : integer;
    begin
        case s is
            when 1 | 2 =>
                x := 3;
            when 3 | 4 | 5 =>
                x := 4;
            when others =>
                x := 5;
        end case;
        wait;
    end process;

end architecture;
