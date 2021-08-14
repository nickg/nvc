entity issue95 is
end entity;

architecture behav of issue95 is

    type point is record
        x : integer;
        y : integer;
        z : boolean;
    end record point;

    type point_array is array (natural range <>) of point;
    signal points : point_array(1 to 4) := (others => (x => 1, y => 1, z => true));

    procedure update(signal pa : out point_array) is
    begin
        pa(3).x <= 7;
    end procedure;

begin
    p1: process
    begin
        assert points(3).x = 1;
        points(2).y <= 4;
        wait for 2 ns;
        update(points);
        wait;
    end process;

    p2: process is
    begin
        wait for 1 ns;
        assert points(2) = (1, 4, true);
        wait for 2 ns;
        assert points(3) = (7, 1, true);
        wait;
    end process;
end behav;
