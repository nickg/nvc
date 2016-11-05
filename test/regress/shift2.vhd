entity shift2 is
end entity;

architecture test of shift2 is
    signal input : bit_vector(4 downto 0) := "11100";
begin

        assert bit_vector'(input) ror -8  = "00111"
            report "ror -8 is broken" severity error;

        assert bit_vector'(input) ror -7  = "10011"
            report "ror -7 is broken" severity error;

        assert bit_vector'(input) ror -6  = "11001"
            report "ror -6 is broken" severity error;

        assert bit_vector'(input) ror -5  = input
            report "ror -5 is broken" severity error;

        assert bit_vector'(input) ror -4  = "01110"
            report "ror -4 is broken" severity error;

        assert bit_vector'(input) ror -3  = "00111"
            report "ror -3 is broken" severity error;

        assert bit_vector'(input) ror -2  = "10011"
            report "ror -2 is broken" severity error;

        assert bit_vector'(input) ror -1  = "11001"
            report "ror -1 is broken" severity error;

        assert bit_vector'(input) ror  0  = input
            report "ror  0 is broken" severity error;

        assert bit_vector'(input) ror  1  = "01110"
            report "ror  1 is broken" severity error;

        assert bit_vector'(input) ror  2  = "00111"
            report "ror  2 is broken" severity error;

        assert bit_vector'(input) ror  3  = "10011"
            report "ror  3 is broken" severity error;

        assert bit_vector'(input) ror  4  = "11001"
            report "ror  4 is broken" severity error;

        assert bit_vector'(input  ror  5)  = input
            report "ror  5 is broken" severity error;

        assert bit_vector'(input) ror  6  = "01110"
            report "ror  6 is broken" severity error;

        assert bit_vector'(input) ror  7  = "00111"
            report "ror  7 is broken" severity error;

        assert bit_vector'(input) ror  8  = "10011"
            report "ror  8 is broken" severity error;

    -- ROL

        assert bit_vector'(input) rol -8  = "10011"
            report "rol -8 is broken" severity error;

        assert bit_vector'(input) rol -7  = "00111"
            report "rol -7 is broken" severity error;

        assert bit_vector'(input) rol -6  = "01110"
            report "rol -6 is broken" severity error;

        assert bit_vector'(input  rol -5)  = input
            report "rol -5 is broken" severity error;

        assert bit_vector'(input) rol -4   = "11001"
            report "rol -4 is broken" severity error;

        assert bit_vector'(input) rol -3  = "10011"
            report "rol -3 is broken" severity error;

        assert bit_vector'(input) rol -2  = "00111"
            report "rol -2 is broken" severity error;

        assert bit_vector'(input) rol -1  = "01110"
            report "rol -1 is broken" severity error;

        assert bit_vector'(input) rol  0  = input
            report "rol  0 is broken" severity error;

        assert bit_vector'(input) rol  1  = "11001"
            report "rol  1 is broken" severity error;

        assert bit_vector'(input) rol  2  = "10011"
            report "rol  2 is broken" severity error;

        assert bit_vector'(input) rol  3  = "00111"
            report "rol  3 is broken" severity error;

        assert bit_vector'(input) rol  4  = "01110"
            report "rol  4 is broken" severity error;

        assert bit_vector'(input) rol  5  = input
            report "rol  5 is broken" severity error;

        assert bit_vector'(input) rol  6  = "11001"
            report "rol  6 is broken" severity error;

        assert bit_vector'(input) rol  7  = "10011"
            report "rol  7 is broken" severity error;

        assert bit_vector'(input) rol  8  = "00111"
            report "rol  8 is broken" severity error;

end architecture;
