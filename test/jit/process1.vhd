entity process1 is
end entity;

architecture test of process1 is
begin

    p1: process is
    begin
        report "hello, world";
        wait;
    end process;

end architecture;
