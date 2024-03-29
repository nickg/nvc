entity array19 is
    generic ( N : integer := 2 );
end entity;

architecture test of array19 is
    subtype t_two is integer range 1 to N;
    type t_enum is (a, b);
    type t_table is array (t_two, t_two, t_enum) of boolean;

    constant table : t_table := (
        1 => ( 1 => (others => true),
               2 => (others => false) ),
        2 => ( 1 => (others => false),
               2 => (others => true ) ) );

begin

    check: process is
        variable v : bit_vector(31 downto 0);
        variable i : integer := 3;
    begin
        assert table(2, 1, a) = false;
        assert table(1, 1, b) = true;

        v(i*8+7 downto i*8) := (i*8+7 downto i*8 => '1');
        assert v = X"FF000000";

        wait;
    end process;

end architecture;
