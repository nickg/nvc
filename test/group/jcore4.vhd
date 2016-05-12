entity jcore4 is
end entity;

architecture test of jcore4 is
    type rt is record
        x : bit_vector(1 to 3);
    end record;

    type at is array (integer range <>) of rt;

    signal a : at(1 to 3);
begin
end architecture;
