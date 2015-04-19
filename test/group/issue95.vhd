entity issue95 is
end entity;

architecture behav of issue95 is

    type point is record
        x : integer;
        z : boolean;
    end record point;

    type point_array is array(0 to 2) of point;
    signal points : point_array := (others => (x => 1, z => true));

begin
end behav;
