package issue247 is
    subtype natural_down is natural range 10 downto 0;
    type array_t is array (natural_down range <>) of boolean;
    constant c : array_t(9 downto 5);   -- ok
end package issue247;
