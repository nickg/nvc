entity bounds44 is
end entity;

architecture test of bounds44 is
    function get_bits(b : bit; n : natural) return bit_vector is
    begin
        return (1 to n => b);
    end function;
begin

    b: block is
        port ( p : in bit_vector(1 to 2) );
        port map ( p => inertial get_bits('1', 5) );
    begin
    end block;

end architecture;
