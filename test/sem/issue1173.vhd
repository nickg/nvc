package test_pkg is

    type t_test is record
        sig        : bit;
        sig_vector : bit_vector(15 downto 0);
    end record;

    constant C_WIDTH : integer := t_test.sig_vector'length;  -- OK (relaxed)

    constant C_CONSTANT : bit_vector(t_test.sig_vector'range) := (others => '1');

    type t_test_2 is record
        sig        : bit;
        sig_vector : bit_vector(t_test.sig_vector'range);
    end record;

    constant c_error : bit := t_test_2.sig;  -- Error

end package;
