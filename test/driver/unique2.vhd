entity unique2 is
end entity;

architecture test of unique2 is
    signal s1 : bit_vector(1 downto 0);

    impure function get_limit return integer is
    begin
        return s1'length;
    end function;

    constant limit : integer := get_limit;
begin
    g: for i in 0 to limit generate
        s1(i) <= '1';
    end generate;
end architecture;
