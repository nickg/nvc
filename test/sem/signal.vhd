entity e is
end entity;

architecture a of e is
    signal v       : bit_vector(1 to 3);
    signal x, y, z : bit;
begin

    (x, y, z) <= v;                     -- OK (TODO)

    process is
    begin
        (x, y, z) <= v;                 -- OK (TODO)
    end process;

end architecture;
