package issue1257 is
    constant a : bit_vector'element := '1';  -- OK

    constant DATA_WIDTH: positive := 32;
    type logic_array_t is array (natural range <>) of bit_vector(DATA_WIDTH - 1 downto 0);

    constant b : logic_array_t'element := (others => '0');  -- OK
end package;
