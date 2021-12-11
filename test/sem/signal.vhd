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

    b1: block is
        procedure proc1 (signal s : out bit) is
            procedure nested is
            begin
                s <= '0';               -- OK
            end procedure;
        begin
            x <= '1';                   -- Error
            s <= '1';                   -- OK
        end procedure;
    begin
    end block;

    b2: block (true) is
    begin
        guard <= false;                 -- Error
    end block;

    b3: block is
        signal guard : integer;
    begin
        x <= guarded not x;             -- Error
    end block;

    b4: block (v) is                    -- Error
    begin
    end block;

    b5: block is
        constant guard : boolean := false;
    begin
        x <= guarded not x;             -- Error
        x <= null;                      -- Error
    end block;

    b6: block is
        signal q : integer bus;         -- Error
    begin
    end block;

end architecture;
