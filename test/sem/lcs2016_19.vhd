entity lcs2016_19 is
end entity;

architecture test of lcs2016_19 is
    signal s1 : integer_vector := (1, 2, 3);  -- OK
    signal s2 : integer_vector;         -- Error
begin

    p1: process is
        variable v1 : integer_vector := (1, 2, 3);  -- OK
        variable v2 : integer_vector;   -- Error
    begin
        wait;
    end process;

end architecture;
