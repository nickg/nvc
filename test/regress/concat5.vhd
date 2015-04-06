entity concat5 is
end entity;

architecture test of concat5 is

    function count_as(x : string) return integer is
        variable r : integer := 0;
    begin
        for i in 1 to x'length loop
            if x(i) = 'a' then
                r := r + 1;
            end if;
        end loop;
        return r;
    end function;

begin

    process is
        variable x : string(1 to 3) := "aba";
    begin
        assert count_as(x & "baa") = 4;
        assert count_as("baa" & x) = 4;
        assert count_as("baa" & 'a') = 3;
        assert count_as(('b', 'a', 'a') & x) = 4;
        wait;
    end process;

end architecture;
