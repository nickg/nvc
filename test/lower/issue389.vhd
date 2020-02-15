package common is
        type example_type is record
                a: bit;
                b: bit_vector(63 downto 0);
                c: bit_vector(31 downto 0);
        end record;
        constant example_init : example_type := (a => '0', others => (others => '0'));
end common;
