entity vlog136 is
end entity;

architecture test of vlog136 is
    component mid is
    end component;
begin
    u_mid : mid;

    check : process is
    begin
        wait for 1 ns;
        report "PASSED";
        wait;
    end process;
end architecture;
