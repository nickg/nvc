entity bounds is
end entity;

architecture test of bounds is
    type foo is range 1 to 5;
    type my_vec1 is array (positive range <>) of integer;
    type my_vec2 is array (foo range <>) of integer;

    signal s : my_vec1(1 to 10);

    function fun(x : in bit_vector(7 downto 0)) return bit;
    procedure proc(x : in bit_vector(7 downto 0));

    type matrix is array (integer range <>, integer range <>) of integer;

    procedure proc2(x : in matrix(1 to 3, 1 to 3));
begin

    process is
        variable a : my_vec1(0 to 10);  -- Error
        variable b : my_vec2(1 to 60);  -- Error
    begin
    end process;

    s(-52) <= 5;                        -- Error
    s(1 to 11) <= (others => 0);        -- Error
    s(0 to 2) <= (others => 0);         -- Error

    process is
    begin
        report (0 => 'a');              -- Error
    end process;

    process is
        variable v1 : bit_vector(3 downto 0);
        variable v2 : bit_vector(8 downto 1);
        variable m1 : matrix(1 to 3, 2 to 4);
        variable m2 : matrix(1 to 3, 1 to 4);
    begin
        assert fun(v1) = '1';           -- Error
        proc(v1);                       -- Error
        proc(v2);                       -- OK
        proc2(m1);                      -- OK
        proc2(m2);                      -- Error
    end process;

end architecture;
