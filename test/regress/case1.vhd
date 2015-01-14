entity case1 is
end entity;

architecture test of case1 is
begin

    process is
        variable x : integer;
    begin
        x := 5;
        wait for 1 ns;
        case x is
            when 1 =>
                assert false;
            when 5 =>
                report "five!";
            when others =>
                assert false;
        end case;
        wait;
    end process;

end architecture;
