entity process1 is
end entity;

architecture test of process1 is
begin

    p1: process is
        variable x : integer;
    begin
        report "hello, world";
        x := 42;
        wait for 1 ns;
        report "after 1 ns";
        x := x + 1;
        wait;
    end process;

end architecture;
