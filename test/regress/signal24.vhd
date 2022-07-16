entity signal24 is
end entity;

architecture test of signal24 is
    type rec is record
        x : integer;
        z : bit_vector(1 to 3);
    end record;

    type rec_array is array (natural range <>) of rec;

    signal s : rec_array(1 to 2);
begin

    p1: process is
    begin
        s(2) <= (5, "101");
        wait for 1 ns;
        assert s = ((integer'left, "000"), (5, "101"));
        wait;
    end process;

end architecture;
