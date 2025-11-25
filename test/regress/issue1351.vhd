entity issue1351 is
end entity;

architecture rtl of issue1351 is
    type t_foo is record
        foo: bit_vector;
    end record;

    type t_bar is array (natural range <>) of t_foo;
    signal data : t_bar(1 downto 0)(foo(3 downto 0));
    signal done : boolean;
begin

    process
    begin
        data(1).foo(0) <= '1' after 1 ns;
        assert data'stable;
        wait until data'stable;
        assert now = 1 ns;
        wait;
    end process;

end architecture;
