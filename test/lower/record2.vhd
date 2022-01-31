entity record2 is
end entity;

architecture test of record2 is

    type r1 is record
        x, y : integer;
    end record;

    type r1_ptr is access r1;

begin

    p1: process is
        variable r : r1;
        variable x : integer;
    begin
        r := (x, x);
        wait;
    end process;

    p2: process is
        variable r : r1_ptr;
    begin
        r := new r1;
        wait;
    end process;

    p3: process is
        variable r : r1_ptr;
        variable x : integer;
    begin
        r := new r1'(x, x);
        wait;
    end process;

end architecture;
