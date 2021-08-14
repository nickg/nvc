entity signal4 is
end entity;

architecture test of signal4 is
    signal s : bit_vector(3 downto 0) := (1 => '1', others => '0');
begin

    p1: process is
        variable v : bit_vector(3 downto 0) := (others => '1');
    begin
        v(2) := s(3);
        s <= v;
        v := s;
        wait;
    end process;

end architecture;
