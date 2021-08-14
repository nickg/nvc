entity slice2 is
end entity;

architecture test of slice2 is

    procedure set_it(signal v : out bit_vector(7 downto 0);
                     x, y     : in integer;
                     k        : in bit_vector) is
    begin
        v(x downto y) <= k;
    end procedure;

    procedure set_it2(signal v : out bit_vector;
                      x, y     : in integer;
                      k        : in bit_vector) is
    begin
        v(x downto y) <= k;
    end procedure;

    signal vec : bit_vector(7 downto 0);

begin

    process is
    begin
        set_it(vec, 3, 0, X"a");
        wait for 1 ns;
        assert vec = X"0a";
        set_it(vec, 4, 1, X"f");
        wait for 1 ns;
        assert vec = X"1e";
        set_it2(vec, 7, 4, X"f");
        wait for 1 ns;
        assert vec = X"fe";
        wait;
    end process;

end architecture;
