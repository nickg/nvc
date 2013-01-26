entity bounds2 is
end entity;

architecture test of bounds2 is

    type int_vec is array (natural range <>) of integer;

begin

    process is
        variable v : int_vec(1 to 10) := (others => 0);
    begin
        v(51761) := 2;
        wait;
    end process;

end architecture;
