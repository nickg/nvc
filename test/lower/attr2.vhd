entity attr2 is
end entity;

architecture test of attr2 is
    type rec is record
        f : bit_vector;
    end record;

    type rec_array is array (natural range <>) of rec;

    impure function get_k return integer is
    begin
        return 3;
    end function;

    constant k : integer := get_k;
    signal r : rec_array(1 to k)(f(1 to k));
    constant l : integer := r(k).f'length;
begin

end architecture;
