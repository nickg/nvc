entity array5 is
end entity;

architecture test of array5 is

    type int_vec is array (integer range <>) of integer;

    procedure negative_range (x : out int_vec;
                              y : in int_vec) is
    begin
        x(0 downto -x'length + 1) := y;
    end procedure;

begin

    main: process is
        variable a : int_vec(0 downto -3);
        variable b : int_vec(0 to 3);
    begin
        b := (1, 2, 3, 4);
        negative_range(a, b);
        assert a = (1, 2, 3, 4);
        wait;
    end process;

end architecture;
