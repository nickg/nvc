entity sub is
end entity;

architecture test of sub is
    signal x : integer;
begin
    check: process is
    begin
        wait for 1 ns;
        assert x = 42;
        wait;
    end process;
end architecture;

entity issue884 is
end entity;

architecture test of issue884 is
begin

    uut: entity work.sub;

    stim: process is
        procedure proc is
        begin
            << signal .issue884.uut.x : integer >> <= 42;
        end procedure;
    begin
        proc;
        wait;
    end process;

end architecture;
