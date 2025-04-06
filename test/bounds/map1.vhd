entity map1 is
end entity;

architecture test of map1 is
    constant c : bit_vector(1 to 99) := (others => '0');
    signal s : bit_vector(1 to 50);
    signal i : integer;

    function to_int(x : bit_vector) return integer is
    begin
        return 1;
    end function;

    subtype bv2 is bit_vector(1 to 2);

    function to_bv2(x : integer) return bv2 is
        constant c1 : bit_vector(1 to 3) := "111";
    begin
        return c1;                      -- XXX
    end function;
begin

    b1: block is
        generic (
            g1 : bit_vector(1 to 3);
            g2 : integer range 0 to 3 );
        generic map (
            g1 => c(1 to 4),            -- Error
            g2 => 7 );                  -- Error
        port (
            p1 : bit_vector(1 to 3) );
        port map (
            p1 => s(1 to 2) );          -- Error
    begin
    end block;

    b2: block is
        port (
            p1 : in bit_vector(1 to 3);
            p2 : in integer;
            p3 : inout bit_vector(1 to 5);
            p4 : inout integer  );
        port map (
            p1 => to_bv2(i),             -- Error
            p2 => to_int(s),             -- OK
            to_int(p3) => to_bv2(i),     -- OK
            to_bv2(p4) => to_int(s) );   -- OK
    begin
    end block;

end architecture;
