entity issue54 is
begin
end entity issue54;

architecture a of issue54 is
begin
    p : process
        variable v : bit_vector(7 downto 0) := (others => '0');
    begin
        v(3 downto 0) := (7 downto 4 => '1');  -- OK
        v(7 downto 4) := (3 downto 0 => '1');  -- OK
        v(7 downto 4) := (3 downto 0 => '1', others => '0');  -- Error
        assert (v = (7 downto 0 => '1'));
        wait;
    end process p;
end architecture a;
