entity repro is
end entity;

architecture rtl of repro is
    signal clk : bit := '0';

    procedure waits_a_bit is
    begin
        wait for 1 ns;
    end procedure;

    procedure outer_proc is
    begin
        for i in 0 to 0 loop
            for j in 0 to 0 loop
                waits_a_bit;
                waits_a_bit;
                return;
            end loop;
        end loop;

        -- unreachable, but still part of the procedure
        report "unreachable" severity note;
    end procedure;
begin

    clk <= not clk after 5 ns;

    process
    begin
        outer_proc;
        report "done" severity note;
        wait;
    end process;

end architecture;
