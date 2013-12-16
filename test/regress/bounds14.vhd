entity bounds14 is
end entity;

architecture test of bounds14 is

    function get_bits(x, len : natural) return bit_vector is
    begin
        return bit_vector'(1 to len => '0');
    end function;

    signal x : bit_vector(7 downto 0) := get_bits(1, 12);  -- Error

begin

end architecture;
