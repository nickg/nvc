entity e is
    port (
        x : in bit_vector(8+23+2 downto 0) );  -- Error with prefer-explicit
end entity;
