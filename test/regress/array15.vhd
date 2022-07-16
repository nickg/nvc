entity array15 is
end entity;

architecture test of array15 is

    function get_bits (n : natural) return bit_vector is
    begin
        return (1 to n => '0');
    end function;

    type bvv is array (natural range <>) of bit_vector;

    procedure do_test (a, b : in natural) is
        constant k : bvv := ( 1 => get_bits(a),
                              2 => get_bits(b) );
    begin
        report to_string(k(1)'length);
        report to_string(k(2)'length);
        assert a = b report "should have failed" severity failure;
    end procedure;

begin

    p1: process is
    begin
        do_test(2, 2);                  -- OK
        do_test(3, 5);                  -- Error
        wait;
    end process;

end architecture;
