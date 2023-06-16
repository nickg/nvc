entity signal31 is
end entity;

architecture test of signal31 is
    type bv_array_t is array (natural range <>) of bit_vector;

    type rec_t is record
        f : bv_array_t;
    end record;

    signal s : rec_t(f(1 to 3)(1 to 2));
begin

    p1: process is
    begin
        s.f(2) <= "10";
        wait for 1 ns;
        assert s.f(2)(1) = '1';
        wait;
    end process;

end architecture;
