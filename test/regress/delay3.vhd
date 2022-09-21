entity delay3 is
end entity;

architecture test of delay3 is
    signal x : bit;
begin

    p1: process is
        variable v : time := 5 ns;
    begin
        x <= reject v inertial '1' after 2 ns;
        wait;
    end process;

end architecture;
