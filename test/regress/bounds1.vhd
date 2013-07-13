entity bounds1 is
end entity;

architecture test of bounds1 is

    type int_vec is array (natural range <>) of integer;

begin

    process is
        variable v : int_vec(1 to 10) := (others => 0);
        variable k : integer;
    begin
        k := 602;
        report integer'image(v(k));
        wait;
    end process;

end architecture;
