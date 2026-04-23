entity vlog139 is
end entity;

architecture test of vlog139 is
    component glbl is end component;
    component mid is end component;
begin
    U_GLBL : glbl;
    U_MID  : mid;

    check : process is
    begin
        wait for 2 ns;
        report "PASSED";
        wait;
    end process;
end architecture;
