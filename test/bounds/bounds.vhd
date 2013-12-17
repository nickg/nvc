entity bounds is
end entity;

architecture test of bounds is
    type foo is range 1 to 5;
    type my_vec1 is array (positive range <>) of integer;
    type my_vec2 is array (foo range <>) of integer;

    signal s : my_vec1(1 to 10);
    signal n : my_vec1(1 downto 10);

    subtype bool_true is boolean range true to true;

    function fun(x : in bit_vector(7 downto 0)) return bit;
    procedure proc(x : in bit_vector(7 downto 0));
    function natfunc(x : in natural) return boolean;
    function enumfunc(x : in bool_true) return boolean;
    function realfunc(x : in real) return boolean;

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

    s <= s(1 to 9);                     -- Error
    n <= s(1 to 2);                     -- Error
    n <= (1, 2, 3);                     -- Error

    process is
        variable v : my_vec1(1 to 3);
    begin
        v := s;                         -- Error
    end process;

    process is
        variable x : integer;
    begin
        x := s(11);                     -- Error!
        x := s(-1);                     -- Error!
    end process;

    process is
        variable a : my_vec1(1 to 3);
    begin
        a := (1, 2, 3);                 -- OK
        a := (5 => 1, 1 => 2, 0 => 3);  -- Error
    end process;

    process is
        subtype alpha is character range 'a' to 'z';
        variable a : alpha;
        variable p : positive;
    begin
        a := 'c';                       -- OK
        a := '1';                       -- Error
        p := 0;                         -- Error
    end process;

    process is
    begin
        assert s'length(5) = 5;         -- Error
    end process;

    process is
    begin
        assert natfunc(-1);             -- Error
    end process;

end architecture;
