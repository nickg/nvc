entity e is
    port (
        p : in bit );
end entity;

architecture a of e is
    signal v       : bit_vector(1 to 3);
    signal x, y, z : bit;
begin

    process is
    begin
        (x, y, z) <= v;                 -- OK
        (x, y, z) <= x;                 -- Error
        (x, y, z) <= "101";             -- Error
        (bit'('1'), y, z) <=  v;        -- Error
        (others => x) <= v;             -- Error
        (p, y, z) <= v;                 -- Error
    end process;

    (x, y, z) <= v;                 -- OK
    (x, y, z) <= x;                 -- Error
    (bit'('1'), y, z) <=  v;        -- Error
    (others => x) <= v;             -- Error
    (p, y, z) <= v;                 -- Error

    process is
        variable i : integer;
    begin
        (v(i), v(1), v(2)) <= v;        -- Error
    end process;

end architecture;
