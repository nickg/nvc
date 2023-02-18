entity wait2 is
end entity;

architecture test of wait2 is
    type rec is record
        x, y : bit;
    end record;

    signal r : rec;
begin

    p1: process is
    begin
        wait until r.x = '1' and r.x'event;
        wait;
    end process;

end architecture;
