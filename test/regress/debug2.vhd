entity debug2 is
end entity;

architecture test of debug2 is

    procedure proc1 is
    begin
        report "proc1" severity warning;
    end procedure;

begin

    p1: process is
        procedure proc2 is
        begin
            proc1;
            report "proc2" severity warning;
        end procedure;
    begin
        report "p1" severity warning;
        proc1;
        proc2;
        wait;
    end process;

    b1: block is
        procedure proc3 is
        begin
            report "proc3" severity warning;
        end procedure;
    begin

        p2: process is
            procedure proc4 is
            begin
                report "proc4" severity warning;
            end procedure;
        begin
            wait for 1 ns;
            proc3;
            proc4;
            proc1;
            report "p2" severity warning;
            wait;
        end process;

    end block;

    g1: if 2 > 1 generate
    begin

        p3: process is
        begin
            wait for 2 ns;
            report "p3" severity warning;
            wait;
        end process;

        g2: for i in 1 to 1 generate
        begin

            p4: process is
            begin
                wait for 3 ns;
                report "p4" severity warning;
                wait;
            end process;

        end generate;

    end generate;

end architecture;
