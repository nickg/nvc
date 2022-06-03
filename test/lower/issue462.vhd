entity issue462 is
end entity;

architecture test of issue462 is

    type mem_t is array (natural range <>) of bit_vector(0 to 7);
    signal s : mem_t(1 to 3);

    function get (m : mem_t; x, y : natural) return bit is
    begin
        return m(x)(y);
    end function;

begin
end architecture;
