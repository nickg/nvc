entity bounds1 is
end entity;

architecture test of bounds1 is

    type int_vec is array (natural range <>) of integer;

begin

    process is
        variable v : int_vec(1 to 10) := (others => 0);
    begin
        report integer'image(v(602));
        wait;
    end process;

end architecture;
