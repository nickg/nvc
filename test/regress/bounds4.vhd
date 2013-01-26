entity bounds4 is
end entity;

architecture test of bounds4 is

    type int_vec is array (natural range <>) of integer;

    procedure check(x : in int_vec) is
    begin
        assert x(5 to 15) = (5 to 15 => 0);
    end procedure;

begin

    process is
        variable v : int_vec(1 to 10) := (others => 0);
    begin
        check(v);
        wait;
    end process;

end architecture;
