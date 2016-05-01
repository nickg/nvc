entity recref1 is
end entity;

architecture test of recref1 is
    type rec is record
        x : bit_vector(1 to 3);
        y : integer;
    end record;

    signal s : rec;                     -- 0..3
    signal t : rec;                     -- 4..7
    signal u : rec;                     -- 8..11
begin
    s.x <= "101";
    s.y <= 4;
end architecture;
