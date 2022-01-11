entity record2 is
end entity;

architecture test of record2 is

    type r1 is record
        x, y : integer;
    end record;

begin

    p1: process is
        variable r : r1;
        variable x : integer;
    begin
        r := (x, x);
        wait;
    end process;

end architecture;
